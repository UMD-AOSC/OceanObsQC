!! author: Travis Sluka
!! category: support
!! Kd-tree creation and search methods

MODULE kdtree
  !! A lon/lat based 3D kd tree for fast retrieval of points.
  !!
  !! Initialization of the tree via kd_init() creates a 3d tree
  !! with points converted into x/y/z space. Once the tree is constructed
  !! points can then be retrieved in O(log n) time either by a point and
  !! max radius with kd_search_radius(), or by a point and
  !! the number of desired closest points to return kd_search_nnearest()
  !!
  !!
  !! @note Algorithm derived from Numerical Recipes, 2007
  !!
  !! @todo Allow for specification of MINDIV during initialization.
  !! Different trees might require different values here
  !!
  !! @todo fix bug where crashes on init if too few observations
  !!
  !! @todo ensure all distance calculations have been optimized (e.g.
  !!   precomupted cos sin of lat/lon)

  IMPLICIT NONE
  PRIVATE


  ! public module methods
  !------------------------------------------------------------
  PUBLIC :: kd_root, kd_init, kd_free
  PUBLIC :: kd_search_radius, kd_search_nnearest


  ! private module parameters
  !------------------------------------------------------------
  INTEGER, PARAMETER :: dp=KIND(0.0d0)
  INTEGER, PARAMETER :: sp=KIND(0.0e0)

  INTEGER, PARAMETER :: kd_dim = 3
  !! 3 dimensions, x/y/z

  INTEGER, PARAMETER :: task_size = 50
  !! size of the tree division and search job stacks

  INTEGER, PARAMETER :: MINDIV = 10
  !! kd-tree will not bother dividing a box any further if it contains
  !! this many or fewer items.

  REAL(dp), PARAMETER :: pi = 4*ATAN(1.0_dp)
  !! pi, duh

  REAL(dp), PARAMETER :: re = 6371.3d3
  !! radius of earth


  ! custom types
  !------------------------------------------------------------
  TYPE kd_root
     !! wrapper for all the information needed by the kd-tree after
     !! creation and during searching. Object is created by kd_init()
     !! to be used by kd_search_radius() and kd_search_nnearest().
     !! The User shouldn't need to access any of these members directly

     INTEGER, POINTER :: ptindx(:)
     !! array holding pointers to the lat/lon array indexes, this
     !! is the array that is organized by the creation of the kd tree

     TYPE(boxnode), POINTER :: boxes(:)
     !! collection of all boxes created for the kd tree

     REAL(dp), POINTER :: pts(:,:)
     !! array of all points stored in the kd tree, in x/y/z form

     REAL(dp), POINTER :: pts_ll(:,:)
     !! array of all points stored in the kd tree, in original lon/lat form
  END TYPE kd_root



  TYPE boxnode
     !! A node for defining each box used by the kd-tree.
     !! The end-user should normally be able to ignore what is in this object

     REAL(dp) :: lo(kd_dim), hi(kd_dim)
     !! lower and higher bound of this box, in x/y/z space

     INTEGER :: mom
     !! index of parent box

     INTEGER :: dau1
     !! index of 1st daughter box, or 0 if no daughter

     INTEGER :: dau2
     !! index of 1st daughter box, or 0 if no daughter

     INTEGER :: ptlo
     !! index within "ptindx" of first point contained within thi box

     INTEGER :: pthi
     !! index within "ptindx" of last point contained within thi box
  END TYPE boxnode




CONTAINS



  !================================================================================
  !================================================================================



  SUBROUTINE kd_free(root)
    !! Frees up any resources associated with a kd tree created with kd_init

    TYPE(KD_ROOT), INTENT(inout)  :: root

    DEALLOCATE(root%ptindx)
    DEALLOCATE(root%pts_ll)
    DEALLOCATE(root%pts)
    DEALLOCATE(root%boxes)
  END SUBROUTINE kd_free



  !================================================================================
  !================================================================================



  SUBROUTINE kd_init(root, lons, lats)
    !! Initialize a kd-tree structure given a list of lat/lon pairs.
    !! The lons and lats variables are copied internally by the module and so
    !! can be deleted after calling this subroutine.

    TYPE(KD_ROOT), INTENT(out)  :: root
    !! the root node of our kd-tree, used by kd_search_nnearest() and kd_search_radius()

    REAL(sp), INTENT(in), TARGET :: lats(:)
    !! list of latitudes (degrees)

    REAL(sp), INTENT(in), TARGET :: lons(:)
    !! list of longitudes (degrees). These can be within any range.

    ! variables used for kd-tree creation loop... too lazy to document what they all are, sorry.
    INTEGER :: kk, np, ntmp, n, m, nboxes, nowtask, ptlo, pthi
    INTEGER :: tmom, tdim, jbox
    REAL(dp), POINTER :: cp(:)
    INTEGER,  POINTER :: hp(:)
    REAL(dp) :: lo(kd_dim), hi(kd_dim)
    INTEGER  :: taskmom(50), taskdim(50)


    ! generate initial unsorted index array
    ALLOCATE(root%ptindx(SIZE(lons)))
    DO n=1, SIZE(root%ptindx)
       root%ptindx(n) = n
    END DO

    ! convert lon/lat to an internally stored x/y/z
    ALLOCATE(root%pts_ll( SIZE(lons), 2))
    ALLOCATE(root%pts( SIZE(lons), kd_dim))
    DO n=1, SIZE(lons)
       root%pts_ll(n,1) = lons(n)*pi/180.0e0
       root%pts_ll(n,2) = lats(n)*pi/180.0e0
       root%pts(n,:) = ll2xyz( lons(n)*1.0_dp, lats(n)*1.0_dp )
    END DO

    ! calculate the number of kd boxes needed and create memory for them
    m = 1
    ntmp = SIZE(root%ptindx)
    DO WHILE (ntmp > 0)
       ntmp = ISHFT(ntmp, -1)
       m = ISHFT(m,1)
    END DO
    nboxes = 2*SIZE(root%ptindx)-ISHFT(m, -1)
    IF (m<nboxes) nboxes = m
    ALLOCATE(root%boxes(nboxes))

    ! initialize the root box, and put its subdivision on the task list
    lo = (/-1e20, -1e20, -1e20/)
    hi = (/ 1e20,  1e20,  1e20/)
    root%boxes(1) = boxnode(lo,hi,0,0,0,1,SIZE(root%ptindx))

    !if we were give a small list, just quit now, there is nothing to divide
    IF(SIZE(root%ptindx) < MINDIV) RETURN

    !otherwise start splitting up the tree
    jbox = 1
    taskmom(1) = 1
    taskdim(1) = 0
    nowtask = 1

    DO WHILE(nowtask > 0)
       ! get the box sitting on the top of the task list
       tmom = taskmom(nowtask)
       tdim = taskdim(nowtask)
       nowtask = nowtask - 1
       ptlo = root%boxes(tmom)%ptlo
       pthi = root%boxes(tmom)%pthi
       hp => root%ptindx(ptlo:pthi)

       ! rotate division among x/y/z coordinates
       cp => root%pts(:,tdim+1)

       ! determine dividing points
       np = pthi - ptlo + 1 ! total points
       kk = (np+1)/2        ! leftmost point of first subdivision

       ! do the array partitioning
       CALL kd_selecti(kk, hp, cp)

       ! create the daughters and push them onto the stack
       ! list if they need further subdivision
       hi = root%boxes(tmom)%hi
       lo = root%boxes(tmom)%lo
       lo(tdim+1) = cp(hp(kk))
       hi(tdim+1) = lo(tdim+1)
       root%boxes(jbox+1) = boxnode(root%boxes(tmom)%lo, hi, tmom, 0, 0, ptlo, ptlo+kk-1)
       root%boxes(jbox+2) = boxnode(lo, root%boxes(tmom)%hi, tmom, 0, 0, ptlo+kk, pthi)
       jbox = jbox+2
       root%boxes(tmom)%dau1 = jbox-1
       root%boxes(tmom)%dau2 = jbox

       ! subdivide the left further
       IF (kk > MINDIV) THEN
          nowtask = nowtask + 1
          taskmom(nowtask) = jbox-1
          taskdim(nowtask) = MOD(tdim+1, kd_dim)
       END IF

       ! subdivide the right further
       IF (np-kk > MINDIV+2) THEN
          nowtask = nowtask + 1
          taskmom(nowtask) = jbox
          taskdim(nowtask) = MOD(tdim+1, kd_dim)
       END IF
    END DO
  END SUBROUTINE kd_init



  !================================================================================
  !================================================================================



  SUBROUTINE kd_search_radius(root,  s_lon, s_lat, s_radius, r_points, r_distance, r_num, exact)
    !! searches for all the points within a given radius.
    !! Maximum number of points to search for depends on the size of "r_points" and "r_distance".
    !! Once these arrays are full the subroutine will exit early.

    TYPE(kd_root), INTENT(in) :: root
    !! root node containing all the information about the kd-tree

    REAL(sp), INTENT(in) :: s_lon
    !! The longitude of the center of the search location (degrees)

    REAL(sp), INTENT(in) :: s_lat
    !! The latitude of the center of the search location (degrees)

    REAL(sp), INTENT(in) :: s_radius
    !! the radius of the search (meters)

    INTEGER, INTENT(out) :: r_points(:)
    !! the resulting list of points that are found,
    !! this is an array of indexes pointing to the original lat/lon arrays passed into kd_init().
    !! Array passed in should be the same size as "r_distance".

    REAL(sp), INTENT(out) :: r_distance(:)
    !! the distance (meters) between each found point and the given search point.
    !! Array passed in should be the same size as "r_points"

    INTEGER, INTENT(out) :: r_num
    !! the number of resulting points that were found.
    !! r_points[1:r_num] and r_distance[1:r_num] are populated after calling this subroutine

    LOGICAL, INTENT(in), OPTIONAL :: exact
    !! if true, the exact great circle distances will be calculated (slower). Otherwise
    !! the euclidean distances are calculated (faster). The faster method
    !! is close enough for most purposes, especially if the search radius is small
    !! compared to the radius of the earth. Default is False.


    ! local variables
    REAL(dp) :: s_xyz(kd_dim)
    REAL(dp) :: r, slatr, clatr, lonr
    INTEGER  :: k, i, n, nb, nbold, ntask, jdim, d1, d2
    INTEGER  :: task(task_size)
    TYPE(boxnode), POINTER :: boxes(:)

    boxes => root%boxes


    ! some basic checks
    IF (SIZE(r_points) /= SIZE(r_distance)) THEN
       WRITE (*,*) "ERROR: kd_search(), r_points and r_distance must be allocated with same size"
       STOP 1
    END IF

    ! convert search point to x/y/z
    s_xyz = ll2xyz(s_lon*1.0_dp, s_lat*1.0_dp)

    ! find the smallest box that completely contains the bounds of the search point
    nb = 1
    jdim = 0
    DO WHILE(boxes(nb)%dau1 /= 0)
       nbold = nb
       d1 = boxes(nb)%dau1
       d2 = boxes(nb)%dau2
       IF( s_xyz(jdim+1) + s_radius <=  boxes(d1)%hi(jdim+1)) THEN
          nb = d1
       ELSE IF ( s_xyz(jdim+1) - s_radius >= boxes(d2)%lo(jdim+1)) THEN
          nb = d2
       END IF
       jdim = MOD(jdim+1, kd_dim)
       IF (nb == nbold) EXIT
    END DO

    ! traverse the tree below this starting box, only as needed
    task(1) = nb
    ntask = 1
    r_num = 0

    ! convert search point to radians
    clatr = COS(s_lat*pi/180.0)
    slatr = SIN(s_lat*pi/180.0)
    lonr  = s_lon * pi/180.0

    DO WHILE(ntask /= 0)
       k = task(ntask)
       ntask = ntask - 1

       ! ignore boxes definitely outside the radius
       i = 0
       DO n = 1, kd_dim
          IF( (boxes(k)%lo(n) - (s_xyz(n)+s_radius)) * (boxes(k)%hi(n) - (s_xyz(n)-s_radius)) > 0) THEN
             i = 1
             EXIT
          END IF
       END DO
       IF (i == 1) CYCLE

       IF(boxes(k)%dau1 /= 0) THEN
          ! process child boxes
          task(ntask+1) = boxes(k)%dau1
          task(ntask+2) = boxes(k)%dau2
          ntask = ntask + 2
       ELSE
          ! process points in this box
          DO i = boxes(k)%ptlo, boxes(k)%pthi
             n = root%ptindx(i)

             ! calculate distance, either great-circle (slower) or
             ! Euclidean (faster)
             IF (PRESENT(exact) .AND. exact) THEN
                ! calculate great-circle distance
                r = dist_gc(lonr, clatr, slatr, root%pts_ll(n,1), root%pts_ll(n,2))
             ELSE
                ! Euclidean distance, should be close enough to great-circle
                ! distance if the search radius is small enough, plus its much
                ! faster. Euclidean distance is smaller than greatcircle, and so more
                ! points will be included here
                r = dist_euc(s_xyz, root%pts(n,:))
             END IF


             ! a new point was found
             IF ( r <= s_radius) THEN
                !make sure there's room for the new found point
                IF (r_num == SIZE(r_points)) THEN
                   WRITE (*,*) "ERROR: more points were found than there was room for in kd_search().",&
                        "Increase the size of the r_points and r_distance arrays"
                   RETURN
                END IF

                ! add the obs
                r_num = r_num + 1
                r_points(r_num) = n
                r_distance(r_num) = r
             END IF

          END DO
       END IF
    END DO
  END SUBROUTINE kd_search_radius



  !================================================================================
  !================================================================================



  PURE SUBROUTINE kd_search_nnearest(root, s_lon, s_lat, s_num, r_points, r_distance, r_num, exact)
    !! selects the "s_num" points in the kd tree that are nearest the search point.

    TYPE(kd_root), INTENT(in) :: root
    !! root node containing all the information about the kd-tree

    REAL(sp), INTENT(in) :: s_lon
    !! The longitude of the center of the search location (degrees)

    REAL(sp), INTENT(in) :: s_lat
    !! The latitude of the center of the search location (degrees)

    INTEGER, INTENT(in) :: s_num
    !! the max number of points to find,

    INTEGER, INTENT(out) :: r_points(:)
    !! the resulting list of points that are found,
    !! this is an array of indexes pointing to the original lat/lon arrays passed into kd_init

    REAL(sp), INTENT(out) :: r_distance(:)
    !! the distance (meters) between each found point and the given search point

    INTEGER, INTENT(out) :: r_num
    !! the number of resulting points that were found

    LOGICAL, INTENT(in), OPTIONAL :: exact
    !! if true, the exact great circle distances will be calculated (slower). Otherwise
    !! the euclidean distances are calculated (faster). The faster method
    !! is close enough for most purposes, especially if the search radius is small
    !! compared to the radius of the earth. Default is False.


    REAL(dp) :: dn(s_num)
    !! heap, containing distances to points

    INTEGER  :: nn(s_num)
    !! array if point indexes, paired with heap array entries

    INTEGER  :: kp, i, n, ntask, k
    REAL(dp) :: d
    REAL(dp) :: s_xyz(kd_dim)
    INTEGER  :: task(task_size)
    REAL(dp) :: slatr, clatr, lonr

    ! set all entries in the heap to a really big number
    dn = 1e20

    ! convert search point to xyz
    s_xyz = ll2xyz(s_lon * 1.0_dp, s_lat*1.0_dp)

    ! find the smallest mother box with enough points to initialize the heap
    kp = kd_locate(root, s_xyz)
    DO WHILE(root%boxes(kp)%pthi-root%boxes(kp)%ptlo < s_num)
       kp = root%boxes(kp)%mom
    END DO

    ! convert search point to radians
    clatr = COS(s_lat*pi/180.0)
    slatr = SIN(s_lat*pi/180.0)
    lonr  = s_lon * pi/180.0

    ! initialize the heap with the "s_num" closest points
    DO i = root%boxes(kp)%ptlo, root%boxes(kp)%pthi
       n = root%ptindx(i)

       ! calculate distance to point
       IF (PRESENT(exact) .AND. exact) THEN
          ! great cicle distance (slower)
          d = dist_gc(lonr, clatr, slatr, root%pts_ll(n,1), root%pts_ll(n,2))
       ELSE
          ! euclidean distance (faster)
          d = dist_euc( s_xyz, root%pts(n,:))
       END IF

       ! if a closer point was found
       IF (d < dn(1) ) THEN
          dn(1) = d
          nn(1) = n
          IF (s_num > 1) CALL sift_down(dn, nn)
       END IF
    END DO

    ! traverse the tree opening possibly better boxes
    task(1) = 1
    ntask = 1
    DO WHILE (ntask /= 0)
       k = task(ntask)
       ntask = ntask - 1

       !dont reuse the box used to initialize the heap
       IF (k == kp) CYCLE

       ! calculate min distance to point (via euclidean distance)
       ! this is still okay even if "exact" is true and we need to
       ! calculate great-circle distance for the points
       d = dist_box(root%boxes(k), s_xyz)

       IF (d < dn(1)) THEN
          ! found a box with points potentially closer
          IF (root%boxes(k)%dau1 /= 0) THEN
             ! put child boxes on task list
             task(ntask+1) = root%boxes(k)%dau1
             task(ntask+2) = root%boxes(k)%dau2
             ntask = ntask + 2
          ELSE
             ! process points in this box
             DO i=root%boxes(k)%ptlo, root%boxes(k)%pthi
                n = root%ptindx(i)

                ! calculate distance
                IF (PRESENT(exact) .AND. exact) THEN
                   ! great circle distance
                   d = dist_gc(lonr, clatr, slatr, root%pts_ll(n,1), root%pts_ll(n,2))
                ELSE
                   ! euclidean distance
                   d = dist_euc(root%pts(n,:), s_xyz)
                END IF

                IF (d < dn(1)) THEN
                   ! found a closer point, add it to the heap
                   dn(1) = d
                   nn(1) = n
                   IF (s_num > 1) CALL sift_down(dn, nn)
                END IF
             END DO
          END IF

       END IF
    END DO

    ! prepare output
    r_num = s_num
    ! TODO, handle situations where number of points found are less than number requested
    DO n=1,s_num
       r_points(n) = nn(n)
       r_distance(n) = dn(n)
    END DO

  END SUBROUTINE kd_search_nnearest



  !================================================================================
  !================================================================================



  PURE FUNCTION kd_locate(root, point)
    !! Given an arbitrary point, return the index of which kdtree box it is in.
    !! Private module function that shouldn't be called by the end user

    TYPE(kd_root), INTENT(in) :: root
    !! kd tree to search in

    REAL(dp), INTENT(in) :: point(3)
    !! point to search for, in x/y/z space

    INTEGER :: kd_locate
    !! index of resulting kd tree box

    INTEGER :: d1, jdim, nb
    nb = 1
    jdim = 0

    DO WHILE (root%boxes(nb)%dau1 /= 0)
       d1 = root%boxes(nb)%dau1
       IF (point(jdim+1) <= root%boxes(d1)%hi(jdim+1)) THEN
          nb=d1
       ELSE
          nb=root%boxes(nb)%dau2
       END IF

       jdim = MOD(jdim+1, kd_dim)
    END DO

    kd_locate = nb
  END FUNCTION kd_locate



  !================================================================================
  !================================================================================



  PURE SUBROUTINE kd_selecti(k, indx, arr)
    !! Permutes indx[1...n] to make
    !!  arr[indx[1..k-1]] <= arr[indx[k]] <= arr[indx[k+1,n]]
    !!   the array "arr" is not modified
    !! Used in the array partitioning of kd_init()

    INTEGER, INTENT(in) :: k
    INTEGER, INTENT(inout) :: indx(:)
    REAL(dp),INTENT(in) :: arr(:)

    INTEGER :: i,ia,ir,j,l,mid
    REAL(dp) :: a

    ! pulled from numerical recipes, 2007.
    ! minimal documentation, sorry.
    l=1
    ir=SIZE(indx)
    DO WHILE(.TRUE.)
       IF (ir <= l+1) THEN
          IF (ir == l+1 .AND. arr(indx(ir)) < arr(indx(l))) &
               CALL swap(indx(l), indx(ir))
          EXIT
       ELSE
          mid = (l+ir) / 2
          CALL swap(indx(mid), indx(l+1))
          IF (arr(indx(l)) > arr(indx(ir)))   CALL swap(indx(l),indx(ir))
          IF (arr(indx(l+1)) > arr(indx(ir))) CALL swap(indx(l+1),indx(ir))
          IF (arr(indx(l)) > arr(indx(l+1)))  CALL swap(indx(l),indx(l+1))
          i=l+1
          j=ir
          ia=indx(l+1)
          a=arr(ia)
          DO WHILE(.TRUE.)
             i = i +1
             DO WHILE(arr(indx(i)) < a)
                i = i + 1
             END DO
             j = j-1
             DO WHILE(arr(indx(j)) > a)
                j = j - 1
             END DO
             IF (j < i) EXIT
             CALL swap(indx(i), indx(j))
          END DO
          indx(l+1)=indx(j)
          indx(j)=ia
          IF (j >= k) ir=j-1
          IF (j<= k) l = i
       END IF
    END DO
  END SUBROUTINE kd_selecti



  !================================================================================
  !================================================================================



  PURE SUBROUTINE swap(a1, a2)
    !! Convenience function to swap two array indices
    !! Used by kd_selecti()

    INTEGER, INTENT(inout) :: a1, a2

    INTEGER :: a

    a = a1
    a1 = a2
    a2 = a
  END SUBROUTINE swap



  !================================================================================
  !================================================================================



  PURE FUNCTION ll2xyz(lon, lat)
    !! convert a point in longitude/latitude into x/y/z coordinates

    REAL(dp), INTENT(in) :: lon
    !! logitude (degrees)

    REAL(dp), INTENT(in) :: lat
    !! latitude (degrees)

    REAL(dp) :: ll2xyz(3)
    !! resulting x/y/z (meters)

    ll2xyz(1) = re * COS(lat*pi/180.0d0) * COS(lon*pi/180.0d0)
    ll2xyz(2) = re * COS(lat*pi/180.0d0) * SIN(lon*pi/180.0d0)
    ll2xyz(3) = re * SIN(lat*pi/180.0d0)

  END FUNCTION ll2xyz



  !================================================================================
  !================================================================================



  PURE FUNCTION dist_euc(p1, p2)
    !! calculate the euclidean distance between two points

    REAL(dp), INTENT(in) :: p1(:), p2(:)
    !! target points, in x/y/z space

    REAL(dp) :: dist_euc
    !! euclidean distance (meters)

    INTEGER :: n
    REAL(dp) :: r1, r2

    r1 = 0.0d0
    DO n=1,SIZE(p1)
       r2 = p1(n) - p2(n)
       r1 = r1 + (r2*r2)
    END DO
    dist_euc = SQRT(r1)
  END FUNCTION dist_euc



  !================================================================================
  !================================================================================



  PURE FUNCTION dist_gc(lon1, clat1, slat1, lon2, lat2)
    !! calculate the great circle distance between two points

    REAL(dp), INTENT(in) :: lon1, clat1, slat1
    !! point 1 longitude (degrees), cosine of latitude, sine of latitude

    REAL(dp), INTENT(in) :: lon2, lat2
    !! point 2 longitude (degrees), latitude (degrees)

    REAL(dp) :: dist_gc

    dist_gc = re * ACOS(MIN(slat1*SIN(lat2) + clat1*COS(lat2) * COS( (lon2-lon1)), 1.0))

  END FUNCTION dist_gc



  !================================================================================
  !================================================================================



  PURE FUNCTION dist_box(box, p)
    !! if point is inside the box, return 0. Otherwise,
    !! caculate the distance between a point and closest spot on the box

    TYPE(boxnode), INTENT(in) :: box

    REAL(dp), INTENT(in) :: p(kd_dim)

    REAL(dp) :: dist_box, dd
    INTEGER :: n

    dd = 0
    DO n=1,kd_dim
       IF (p(n) < box%lo(n)) dd = dd + (p(n) - box%lo(n))*(p(n) - box%lo(n))
       IF (p(n) > box%hi(n)) dd = dd + (p(n) - box%hi(n))*(p(n) - box%hi(n))
    END DO

    dist_box = SQRT(dd)
  END FUNCTION dist_box



  !================================================================================
  !================================================================================



  PURE SUBROUTINE sift_down(heap, ndx)
    !! maintain a heap by sifting the first item down into its
    !! proper place. "ndx" is altered along with "heap"

    REAL(dp), INTENT(inout) :: heap(:)
    !! the heap

    INTEGER, INTENT(inout)  :: ndx(:)
    !! a second array whose elements are moved around to be kept
    !! matched with those of "heap"

    INTEGER :: n, nn, j, jold, ia
    REAL(dp) :: a

    nn = SIZE(heap)
    n = nn
    a = heap(1)
    ia = ndx(1)
    jold = 1
    j = 2
    DO WHILE (j <= n)
       IF ( j < n ) THEN
          IF ( heap(j) < heap(j+1)) j = j+1
       END IF
       IF (a >= heap(j)) EXIT
       heap(jold) = heap(j)
       ndx(jold) = ndx(j)
       jold = j
       j = 2*j
    END DO
    heap(jold) = a
    ndx(jold) = ia
  END SUBROUTINE sift_down



  !================================================================================

END MODULE kdtree

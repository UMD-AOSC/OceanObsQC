PROGRAM obsqc
  USE obs_reader_plugins
  USE obs_writer_plugins

  IMPLICIT NONE

  PRINT *, "Ocean Observation Qualtiy Control"
  PRINT *, ""
  PRINT *, "Available obs reader plugins:"
  CALL register_obs_reader_plugins()

  PRINT *, ""
  PRINT *, "Available obs writer plugins:"
  CALL register_obs_writer_plugins()

  PRINT *, ""
  PRINT *, "Reading profiles"
  CALL obs_reader_init()

END PROGRAM obsqc

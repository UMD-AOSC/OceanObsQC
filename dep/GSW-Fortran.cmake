set(GSW_SRC "")

set( GSW_TOOLBOX_SRC
  gsw_add_barrier.f90
  gsw_add_mean.f90
  gsw_adiabatic_lapse_rate_from_ct.f90
  gsw_adiabatic_lapse_rate_ice.f90
  gsw_alpha.f90
  gsw_alpha_on_beta.f90
  gsw_alpha_wrt_t_exact.f90
  gsw_alpha_wrt_t_ice.f90
  gsw_beta_const_t_exact.f90
  gsw_beta.f90
  gsw_cabbeling.f90
  gsw_c_from_sp.f90
  gsw_chem_potential_water_ice.f90
  gsw_chem_potential_water_t_exact.f90
  gsw_cp_ice.f90
  gsw_ct_first_derivatives.f90
  gsw_ct_first_derivatives_wrt_t_exact.f90
  gsw_ct_freezing_exact.f90
  gsw_ct_freezing.f90
  gsw_ct_freezing_first_derivatives.f90
  gsw_ct_freezing_first_derivatives_poly.f90
  gsw_ct_freezing_poly.f90
  gsw_ct_from_enthalpy_exact.f90
  gsw_ct_from_enthalpy.f90
  gsw_ct_from_entropy.f90
  gsw_ct_from_pt.f90
  gsw_ct_from_rho.f90
  gsw_ct_from_t.f90
  gsw_ct_maxdensity.f90
  gsw_ct_second_derivatives.f90
  gsw_deltasa_atlas.f90
  gsw_deltasa_from_sp.f90
  gsw_dilution_coefficient_t_exact.f90
  gsw_dynamic_enthalpy.f90
  gsw_enthalpy_ct_exact.f90
  gsw_enthalpy_diff.f90
  gsw_enthalpy.f90
  gsw_enthalpy_first_derivatives_ct_exact.f90
  gsw_enthalpy_first_derivatives.f90
  gsw_enthalpy_ice.f90
  gsw_enthalpy_second_derivatives_ct_exact.f90
  gsw_enthalpy_second_derivatives.f90
  gsw_enthalpy_sso_0.f90
  gsw_enthalpy_t_exact.f90
  gsw_entropy_first_derivatives.f90
  gsw_entropy_from_pt.f90
  gsw_entropy_from_t.f90
  gsw_entropy_ice.f90
  gsw_entropy_part.f90
  gsw_entropy_part_zerop.f90
  gsw_entropy_second_derivatives.f90
  gsw_fdelta.f90
  gsw_frazil_properties.f90
  gsw_frazil_properties_potential.f90
  gsw_frazil_properties_potential_poly.f90
  gsw_frazil_ratios_adiabatic.f90
  gsw_frazil_ratios_adiabatic_poly.f90
  gsw_geo_strf_dyn_height.f90
  gsw_geo_strf_dyn_height_pc.f90
  gsw_gibbs.f90
  gsw_gibbs_ice.f90
  gsw_gibbs_ice_part_t.f90
  gsw_gibbs_ice_pt0.f90
  gsw_gibbs_ice_pt0_pt0.f90
  gsw_gibbs_pt0_pt0.f90
  gsw_grav.f90
  gsw_helmholtz_energy_ice.f90
  gsw_hill_ratio_at_sp2.f90
  gsw_ice_fraction_to_freeze_seawater.f90
  gsw_internal_energy.f90
  gsw_internal_energy_ice.f90
  gsw_ipv_vs_fnsquared_ratio.f90
  gsw_kappa_const_t_ice.f90
  gsw_kappa.f90
  gsw_kappa_ice.f90
  gsw_kappa_t_exact.f90
  gsw_latentheat_evap_ct.f90
  gsw_latentheat_evap_t.f90
  gsw_latentheat_melting.f90
  gsw_linear_interp_sa_ct.f90
  gsw_melting_ice_equilibrium_sa_ct_ratio.f90
  gsw_melting_ice_equilibrium_sa_ct_ratio_poly.f90
  gsw_melting_ice_into_seawater.f90
  gsw_melting_ice_sa_ct_ratio.f90
  gsw_melting_ice_sa_ct_ratio_poly.f90
  gsw_melting_seaice_equilibrium_sa_ct_ratio.f90
  gsw_melting_seaice_equilibrium_sa_ct_ratio_poly.f90
  gsw_melting_seaice_into_seawater.f90
  gsw_melting_seaice_sa_ct_ratio.f90
  gsw_melting_seaice_sa_ct_ratio_poly.f90
  gsw_mlp.f90
  gsw_nsquared.f90
  gsw_nsquared_lowerlimit.f90
  gsw_nsquared_min_const_t.f90
  gsw_nsquared_min.f90
  gsw_p_from_z.f90
  gsw_pot_enthalpy_from_pt_ice.f90
  gsw_pot_enthalpy_from_pt_ice_poly.f90
  gsw_pot_enthalpy_ice_freezing.f90
  gsw_pot_enthalpy_ice_freezing_first_derivatives.f90
  gsw_pot_enthalpy_ice_freezing_first_derivatives_poly.f90
  gsw_pot_enthalpy_ice_freezing_poly.f90
  gsw_pot_rho_t_exact.f90
  gsw_pressure_coefficient_ice.f90
  gsw_pressure_freezing_ct.f90
  gsw_pt0_cold_ice_poly.f90
  gsw_pt0_from_t.f90
  gsw_pt0_from_t_ice.f90
  gsw_pt_first_derivatives.f90
  gsw_pt_from_ct.f90
  gsw_pt_from_entropy.f90
  gsw_pt_from_pot_enthalpy_ice.f90
  gsw_pt_from_pot_enthalpy_ice_poly_dh.f90
  gsw_pt_from_pot_enthalpy_ice_poly.f90
  gsw_pt_from_t.f90
  gsw_pt_from_t_ice.f90
  gsw_pt_second_derivatives.f90
  gsw_rho_alpha_beta_bsq.f90
  gsw_rho_alpha_beta.f90
  gsw_rho.f90
  gsw_rho_first_derivatives.f90
  gsw_rho_first_derivatives_wrt_enthalpy.f90
  gsw_rho_ice.f90
  gsw_rho_second_derivatives.f90
  gsw_rho_second_derivatives_wrt_enthalpy.f90
  gsw_rho_t_exact.f90
  gsw_rr68_interp_sa_ct.f90
  gsw_saar.f90
  gsw_sa_freezing_estimate.f90
  gsw_sa_freezing_from_ct.f90
  gsw_sa_freezing_from_ct_poly.f90
  gsw_sa_freezing_from_t.f90
  gsw_sa_freezing_from_t_poly.f90
  gsw_sa_from_rho.f90
  gsw_sa_from_sp_baltic.f90
  gsw_sa_from_sp.f90
  gsw_sa_from_sstar.f90
  gsw_sa_p_inrange.f90
  gsw_seaice_fraction_to_freeze_seawater.f90
  gsw_sigma0.f90
  gsw_sigma1.f90
  gsw_sigma2.f90
  gsw_sigma3.f90
  gsw_sigma4.f90
  gsw_sound_speed.f90
  gsw_sound_speed_ice.f90
  gsw_sound_speed_t_exact.f90
  gsw_specvol_alpha_beta.f90
  gsw_specvol_anom_standard.f90
  gsw_specvol.f90
  gsw_specvol_first_derivatives.f90
  gsw_specvol_first_derivatives_wrt_enthalpy.f90
  gsw_specvol_ice.f90
  gsw_specvol_second_derivatives.f90
  gsw_specvol_second_derivatives_wrt_enthalpy.f90
  gsw_specvol_sso_0.f90
  gsw_specvol_t_exact.f90
  gsw_sp_from_c.f90
  gsw_sp_from_sa_baltic.f90
  gsw_sp_from_sa.f90
  gsw_sp_from_sk.f90
  gsw_sp_from_sr.f90
  gsw_sp_from_sstar.f90
  gsw_spiciness0.f90
  gsw_spiciness1.f90
  gsw_spiciness2.f90
  gsw_sr_from_sp.f90
  gsw_sstar_from_sa.f90
  gsw_sstar_from_sp.f90
  gsw_t_deriv_chem_potential_water_t_exact.f90
  gsw_t_freezing_exact.f90
  gsw_t_freezing.f90
  gsw_t_freezing_first_derivatives.f90
  gsw_t_freezing_first_derivatives_poly.f90
  gsw_t_freezing_poly.f90
  gsw_t_from_ct.f90
  gsw_t_from_pt0_ice.f90
  gsw_thermobaric.f90
  gsw_turner_rsubrho.f90
  gsw_util_indx.f90
  gsw_util_interp1q_int.f90
  gsw_util_xinterp1.f90
  gsw_z_from_p.f90
  )
foreach( f IN ITEMS ${GSW_TOOLBOX_SRC})
  list(APPEND GSW_SRC ${CMAKE_CURRENT_SOURCE_DIR}/GSW-Fortran/toolbox/${f})
endforeach()


set(GSW_MODULE_SRC
  gsw_mod_baltic_data.f90
  gsw_mod_check_data.f90
  gsw_mod_error_functions.f90
  gsw_mod_freezing_poly_coefficients.f90
  gsw_mod_gibbs_ice_coefficients.f90
  gsw_mod_kinds.f90
  gsw_mod_saar_data.f90
  gsw_mod_sp_coefficients.f90
  gsw_mod_specvol_coefficients.f90
  gsw_mod_teos10_constants.f90
  gsw_mod_toolbox.f90
  )
foreach( f IN ITEMS ${GSW_MODULE_SRC})
  list(APPEND GSW_SRC ${CMAKE_CURRENT_SOURCE_DIR}/GSW-Fortran/modules/${f})
endforeach()

add_library(gsw STATIC ${GSW_SRC})
set(GSW_INCLUDES ${CMAKE_CURRENT_BINARY_DIR})

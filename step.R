

#  ------------------------------------------------------------------------
#
# Title : step-by step
#    By : PhM
#  Date : 2024-06-10
#
#  ------------------------------------------------------------------------
library(missMDA)


 zz <-  tt |>
  dplyr::select(age_cut,3,9:20,62:65,delai_chirurgical, premier_cut,dernier_cut)

# nn <- estim_ncpFAMD(zz,ncp.max=10)
#nn <- nn$ncp
 nn <- 4
 zzt <- imputeFAMD(zz,ncp=nn)
 zzt <- zzt$completeObs
ll <-  lm(delai_chirurgical~premier_cut + dernier_cut + sexe + type_fracture + diabete + adl_avant_fracture +institution_anterieur + marche_avant_la_chute_avec_aide + bmi + tvp_ep, data = zzt)

 MASS::stepAIC(ll)

 ll <- lm(formula = delai_chirurgical ~ premier_cut + dernier_cut +
      sexe + type_fracture + adl_avant_fracture + marche_avant_la_chute_avec_aide +
      tvp_ep, data = zzt)

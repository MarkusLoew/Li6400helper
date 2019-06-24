#' Recalculates Li6400 gas exchange parameters when leaf area changes using equations from Li6400 computelist
#' 
#' @param data data.frame containing the gas exchange data measured by a Li6400 IRGA
#' @param Area leaf area in [cm] to be used for the recalculation. If not provided (the default) the vector Area from data is used.
#' @param inplace Logical. If FALSE returns a data.frame of the recalculated gas exchange parameters only (default). If TRUE, returns the complete data.frame data with the recalculated values in place of the original values.
#' @return Depending on value of argument inplace above: Data.frame with either just the re-calculated parameters or the same data.frame as data but with the recalculated values for the changed parameters.
# now updated based on computelist from StdComps_6.2
# re-calculation may result in more decimal places bcoming availble for the values compared to the original data from the IRGA.
#' @export

# +++++++++++++++
# Parameters to be recalculated (see Licor 6400 computelist)
#Photo
#Cond
#Ci
#Trmmol
#VpdL
#CTleaf
#BLC_1
#BLCond
#fda
#Trans
#Ti-Ta
#SVTleaf
#h2o_i
#h2o_diff
#CTair
#SVTair
#CndTotal
#CndCO2
#Ci_Pa
#Ci/Ca
#RHsfc
#C2sfc
#AHs/Cs
# +++++++++++++++

Li6400Recalc <- function(data, Area = NA, inplace = FALSE) {

# use area from data frame if no new area is provided as argument
if (is.na(Area)) {
  Area <- data$Area}

# some potential data cleanup
# to take care of a potential "." at the end of the EBal
names(data) <- gsub("\\.$", "", names(data))

# #10 "fda" "flow / area with units conversion"
# " (flow_um * 1E-6) / (area_cm2 * 1E-4 ) "
fda <- data$Flow * 0.000001 / (Area * 0.0001)

# #111 "One sided BLC"
# " area_cm2 * blcSlope + blcOffset "
BLC_1 <- (Area * data$BLCslope + data$BLCoffst)

# #10 "Effective BLC"
# "#111 * (stom_rat + 1) * (stom_rat + 1) / (stom_rat * stom_rat + 1)"
BLCond <- BLC_1 * (data$StmRat + 1) * (data$StmRat + 1) / (data$StmRat * data$StmRat + 1)

# #20 "Trans" "Transpiration (mol/m2/s)" 
# "(h2o_2_mm - h2o_1_mm) / (1000.0 - h2o_2_mm) * #10" 
Trans <- (data$H2OS - data$H2OR) / (1000 - data$H2OS) * fda

# #21 "Trmmol" "Transpiration (mmol/m2/s)"
# #20 * 1E3"
Trmmol <- Trans * 1000

# #2213F1 "Tair_K" "air temp in K"
# tLeaf_c + 273.15"
Tair_K <- (data$Tleaf + 273.15)

# #2241F1 "Twall_K" "Twall temp K"
# tCham_c + 273.15"
Twall_K <- (data$Tair + 273.15)

# #2216 "R(W/m2)" "incoming radiation"
# (parIn_um * f_parIn + parOut_um * f_parOut) * alphaK "
RWm2 <- (data$PARi * data$f_parin + data$PARo * data$f_parout) * data$alphaK

# #2218 "Tl-Ta" "energy balance delta t"
# " (#2216 + 1.0773E-7 * ((#2214 ^ 4) - (#2213 ^ 4)) - #20 * 44100.0)/(#111 * 51.4 + 4.3092E-7 * (#2213 ^ 3)) "
TlTa <-((RWm2 + 0.00000010773 * (Twall_K^4 - Tair_K^4)) / BLC_1 * 0.92 * 2 * 29.3 + 0.00000043092 * Tair_K ^3)

# #221F2 "CTleaf" "Computed leaf temp"
# " Tleaf_c + #2218 * doEB"
CTleaf <- data$Tleaf + TlTa * data$EBal

# #222 "SVTleaf" "SatVap(Tleaf)"
# " ( 0.61365 * EXP(17.502 * #221 / (240.97 + #221))) "
SVTleaf <- 0.61365 * exp(17.502 * CTleaf /(240.97 + CTleaf))

# #223 "h2o_i" "intercellular h2o"
#" #222 * 1000 / press_kPa "
h2o_i <- SVTleaf * 1000 / data$Press

# #224 "h20diff" "diff"
# " #223 - h2o_2_mm"
h2o_diff <- h2o_i - data$H2OS

# #225 "CTair" "Computed chamber air temp"
# " doEB IF Tleaf_c ELSE Tcham_c Tleaf_c + 2 /  THEN "
#CTair <- if (data$EBal != 0) {
#           data$Tleaf
#          } else {
#          (data$Tair + data$Tleaf) / 2}
CTair <- ifelse(data$EBal != 0,
           data$Tleaf,
          (data$Tair + data$Tleaf) / 2)

# #226 "SVTair" "SatVap(Tair)"
#" ( 0.61365 * EXP(17.502 * #225 / (240.97 + #225))) "
SVTair <- 0.61365 * exp(17.502 * CTair / (240.97 + CTair))

# #22 "CndTotal" "Total conductance"
# " $ #224 0 <> IF 1000 #223 h2o_2_mm + 2 / - #224 / #20 * ELSE 0 THEN "
#CndTotal <- if(h2o_diff !=0) { 
#              (1000 - (h2o_i + data$H2OS) / 2) / h2o_diff * Trans
#           } else {
#              0}S
CndTotal <- ifelse(h2o_diff != 0, 
              (1000 - (h2o_i + data$H2OS) / 2) / h2o_diff * Trans,
              0)

# #23 "Cond" "Stomatal cond. (mol/m2/s)" 
# " $ #22 0 <> IF 1.0 1.0 #22 / 1.0 #11 / - / ELSE 0 THEN "
#Cond <- if (CndTotal != 0) {
#          1 / (1 / CndTotal - 1 / BLCond)
#        } else {
#        0}
Cond <- ifelse (CndTotal != 0,
          1 / (1 / CndTotal - 1 / BLCond),
        0)

# #24 "vp_kPa" "vapor pressure chamber air"
#" h2O_2_mm * press_kPa / 1000 "
vp_kPa <- data$H2OS * data$Press / 1000

# #25 "VpdL" "Leaf VPD (SatVap(Tleaf) - eair)"
# " #222 - #24"
VpdL <- (SVTleaf - vp_kPa)

# #27 "VpdA" "Air VPD (SatVap(tair) - eair)"
# " #226 - #24"
VpdA <- SVTair - vp_kPa

# #30 "Photo" "Photosynthesis (\xe6mol/m2/s)"
#" (co2_1_um - co2_2_um * (1000 - h2o_1_mm) / (1000 - h2o_2_mm)) * #10 "
Photo <- (data$CO2R - data$CO2S * (1000 - data$H2OR) / (1000 - data$H2OS)) * fda

# #35 "CndCO2" "Total Conductance to CO2" 
#" 1.0 / (1.6 / #23 + 1.37 / #11)"
CndCO2 <- 1 / (1.6 / Cond + 1.37 / BLCond)

# #36 "Ci" "Intercellular CO2 (\xe6mol/mol)"
#" ((#35 - #20/2) * co2_2_um - #30) / (#35 + #20/2)"
Ci <- ((CndCO2 - Trans / 2) * data$CO2S - Photo) / (CndCO2 + Trans / 2)

# #38 "Ci_Pa" "Intercellular CO2 (Pa)" 
#" #36 * press_kPa * 1E-3"
Ci_Pa <- Ci * data$Press * 0.001

# #39 "Ci/Ca" "Intercellular CO2 / Ambient CO2"
#" #36 / co2_2_um "
CiCa <- Ci / data$CO2S

# #51 "RHsfc"  "Surface Humidity (%)" 
#" (1.0 - (#20 * press_kpa)/#222/#23) * 100"
RHsfc <- (1 - Trans * data$Press / SVTleaf / Cond) * 100

# #52 "C2sfc"  "Surface CO2 (\xe6mol/mol)"
#" co2_2_um - #30 / (#11 / 1.35)"
C2sfc <- (data$CO2S - Photo / (BLCond / 1.35))

# #53 "AHs/Cs" "Ball-Berry parameter " 
#" #30 * #51 /100.0 / #52 "
AHsC2 <- Photo * RHsfc / 100 / C2sfc


if (inplace == FALSE) {
 # in case of separate result table is asked for
	out <- data.frame(
	        Area = Area,
		Photo = Photo,
		Cond = Cond,
		Ci = Ci,
		Trmmol = Trmmol,
		VpdL = VpdL,
		VpdA = VpdA,
		CTleaf = CTleaf,
		BLC_1 = BLC_1,
		BLCond = BLCond,
		fda = fda,
		Trans = Trans,
		TlTa = TlTa,
		SVTleaf = SVTleaf,
		h2o_i = h2o_i,
		h2o_diff = h2o_diff,
		CTair = CTair,
		SVTair = SVTair,
		CndTotal = CndTotal,
		CndCO2 = CndCO2,
		Ci_Pa = Ci_Pa,
		CiCa = CiCa,
		RHsfc = RHsfc,
		C2sfc = C2sfc,
		AHsCs = AHsC2)
	return(out)
} else {
 # in case values should be overwritten in the provided data
                data$Area <- Area
		data$Photo <- Photo
		data$Cond <- Cond
		data$Ci <- Ci
		data$Trmmol <- Trmmol
		data$VpdL <- VpdL
		#VpdA = VpdA,
		data$CTleaf = CTleaf
		data$BLC_1 = BLC_1
		data$BLCond = BLCond
		#fda = fda,
		#Trans = Trans,
		#TlTa = TlTa,
		#SVTleaf = SVTleaf,
		#h2o_i = h2o_i,
		#h2o_diff = h2o_diff,
		#CTair = CTair,
		#SVTair = SVTair,
		#CndTotal = CndTotal,
		#CndCO2 = CndCO2,
		#Ci_Pa = Ci_Pa,
		#CiCa = CiCa,
		#RHsfc = RHsfc,
		#C2sfc = C2sfc,
		#AHsCs = AHsC2)
 return(data)}
}


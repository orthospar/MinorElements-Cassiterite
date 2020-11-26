#Variables
Di <- function(ri, charge, TK){
  #ri = radius of interest, in angstroms (numeric)
  #charge = charge on the cation (numeric, 0 to 6)
  #TK = temperature, Kelvin (numeric)
  
  #constants
  Avo <- 6.02214076E23 #Avogadro's Constant, in mol-1
  R <- 8.31446261815324 #Gas constant in J mol-1 K-1
  e0 <- 4.80320425E-10/(100^(3/2)*1000^(1/2)) #Charge on an electron, in Statcoulmbs, converted to m3/2 kg1/2 S-1
  eps.c <- 9.9 #Static dielectric constant (epsilon) for cassiterite parallel to c-axis, unitless. 
  eps.ab <- 14.5 #Static dielectric constant (epsilon)for cassiterite perpendicular to c-axis, unitless. 
  K <- 212.6E9 #Bulk elastic modulus for cassiterite, in GPa, converted to Pa
  G <- 106.2E9 #Shear elastic modulus for cassiterite, in GPa, converted to Pa
  E <- (9*K*G)/(3*K+G) #Youngs elastic modulus for cassiterite, derived from K and G, assuming isotropic (which it isn't)
  Sn_cass <- 78.76731 #concentration of Sn in cassiterite, in wt% (assumed constant for model)
  r0 <- 0.69E-10 #radius of Sn4+ in 6-fold coordination
  
  #Calculate doubly compensated partition coefficient (D-double prime)
  D00 <- 1 #Normalised partition coefficient

  #charge difference
  Z.diff <- charge-4
  
  #Calculate radius of charge distribution, ri + (diameter of oxygen)
  rho <- (ri+2.76)*10^-10  #in angstroms, converted to m. 
  ri <- ri*10^-10 #convert ri to m
  r.diff <- ri-r0
  
  #strain due to charge difference term
  strain.charge <- exp((-Avo*(e0^2)*(Z.diff^2))/(2*eps.ab*rho*R*TK))
  
  #empirical reponse to charge on Youngs Modulus
  E <- (charge/4)*E
  
  #strain due to size difference term
  strain.size <- exp((-4*pi*Avo*E*(((r0/2)*(r.diff^2))+((1/3)*(r.diff^3))))/(R*TK))
  
  #final partition coefficient
  D <- D00*strain.size*strain.charge
  return(D)
}
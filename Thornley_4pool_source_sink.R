# R version of Thornley allcoation model (TAM) as in Thornley and Parsons 2014. J. Theor. Biol. 342: 1-14
# Rewritten to include elements of Thornley 1998 modelling shoot-root relations. The only way forward (https://doi.org/10.1006/anbo.1997.0529) 
# Author: Steven Higgins
# Author: Re-coded to add source sink dynamics and remove predefined allocation by Liam Langan 20.11.2017
# This version with carbon organ, nitrogen organ, phosphorus organ and water organ 
###############################################################################

## LL NOTE for OSX users:
## running "ssh-add ~/.ssh/id_rsa" in the terminal allows RStudio Github communication via the ssh passkey
## 

# Because in TAM allocation coeffs act on Mx the total biomass, there is no need for a storage pool

## initial values for fractions of structural dry matter in shoot, root, mycor
# thorn_Fxsh_t0 = 0.2;
# thorn_Fxrt_t0 = 0.3;
# thorn_Fxmy_t0 = 0.5;
## initial values for mass of structural dry matter in rt, sh, my
size.factor<-0.1;

thorn_Mxco_t0 = 0.25/size.factor;
thorn_Mxno_t0 = 0.25/size.factor;
thorn_Mxpo_t0 = 0.25/size.factor; #Thornley uses has 0.5
thorn_Mxwo_t0 = 0.25/size.factor; #Thornley uses has 0.5

## intitial values for mass of substrates C,N,P for carbon organ
thorn_Mcs_co_t0 = 0.025/size.factor;
thorn_Mns_co_t0 = 0.01/size.factor;
thorn_Mps_co_t0 = 0.005/size.factor;
thorn_Mws_co_t0 = 0.25/size.factor;

## intitial values for mass of substrates C,N,P for nitrogen organ
thorn_Mcs_no_t0 = 0.025/size.factor;
thorn_Mns_no_t0 = 0.01/size.factor;
thorn_Mps_no_t0 = 0.005/size.factor;
thorn_Mws_no_t0 = 0.25/size.factor;

## intitial values for mass of substrates C,N,P for phosphorus organ
thorn_Mcs_po_t0 = 0.025/size.factor;
thorn_Mns_po_t0 = 0.01/size.factor;
thorn_Mps_po_t0 = 0.005/size.factor;
thorn_Mws_po_t0 = 0.25/size.factor;

## intitial values for mass of substrates C,N,P for water organ
thorn_Mcs_wo_t0 = 0.025/size.factor;
thorn_Mns_wo_t0 = 0.01/size.factor;
thorn_Mps_wo_t0 = 0.005/size.factor;
thorn_Mws_wo_t0 = 0.25/size.factor;

## upktake rates of C,N,P

#default to go back to
base =  0.0002*100; #aDGVM produces numbers around 0.00001 to 0.001 for C_net/plant_mass
thorn_Kc  = base*1.0;
thorn_Kw  = 0.0015;
thorn_Kn  = 0.25;
thorn_Kp  = 0.1;
# thorn_Kw  = base*0.0015;
# thorn_Kn  = base*0.25;
# thorn_Kp  = base*0.1;

#fiddle with these ones
#genernally varying these K by factors of 10% to 200% is probably what we want to do to represent
#limitation/abundance of carbon, water, N, P.
#play with these to convince yourself that allocation responds as you would expect to variation in uptake parameters
#I think its possible to get the model to produce reasonable C:N and N:P ratios

#base =  0.0002*100; #TTR numbers around 0.00001 to 0.001
#thorn_Kc  = base*1.0; #TTR numbers around 0.00001 to 0.001
#thorn_Kw  = base*0.00015;
#thorn_Kn  = base*0.25;
#thorn_Kp  = base*0.1;

#product inhibittion, not used
#thorn_Jcs = 0.03;
#thorn_Jns = 0.01;
#thorn_Jps = 0.005;
#thorn_Jws = 0.003;


## Fractions of C, N, P in structural dry matter 
#first three are as in Thornley, Fwx is calibrated, higher values do not work
# so Fwx is artificial. Maybe we have to think of this as water used in metabolism?
thorn_Fcx = 0.4;
thorn_Fnx = 0.02;
thorn_Fpx = 0.005;
thorn_Fwx = 0.0001;


## other constants
thorn_q = 0.5;     #control parameter for allocation
#thorn_Kg = 200	; #structural growth rate constant; Thornley uses 200000
thorn_Kg = 0.1	; #structural growth rate constant; Thornley uses 200000

mycol=c("green","blue","black","red")

#---start model----

#set initial states
#for aDGVM this will have to go in the section that creates a new individual
## STATE VARIABLE initial values for mass of structural dry matter in rt, sh, my
thorn_Mxco = thorn_Mxco_t0;
thorn_Mxwo = thorn_Mxwo_t0;
thorn_Mxno = thorn_Mxno_t0;
thorn_Mxpo = thorn_Mxpo_t0;
## STATE VARIABLE initial values for mass of substrate C,N,P
thorn_Mcs_co = thorn_Mcs_co_t0;
thorn_Mns_co = thorn_Mns_co_t0;
thorn_Mps_co = thorn_Mps_co_t0;
thorn_Mws_co = thorn_Mws_co_t0;

thorn_Mcs_no = thorn_Mcs_no_t0;
thorn_Mns_no = thorn_Mns_no_t0;
thorn_Mps_no = thorn_Mps_no_t0;
thorn_Mws_no = thorn_Mws_no_t0;

thorn_Mcs_po = thorn_Mcs_po_t0;
thorn_Mns_po = thorn_Mns_po_t0;
thorn_Mps_po = thorn_Mps_po_t0;
thorn_Mws_po = thorn_Mws_po_t0;

thorn_Mcs_wo = thorn_Mcs_po_t0;
thorn_Mns_wo = thorn_Mns_po_t0;
thorn_Mps_wo = thorn_Mps_po_t0;
thorn_Mws_wo = thorn_Mws_po_t0;

thorn_Mx_t0 = thorn_Mxco_t0 + thorn_Mxno_t0 + thorn_Mxpo_t0 + thorn_Mxwo_t0;

## AUX VARIABLE initial values for concentrations of substrates C,N,P
thorn_Cs_co = thorn_Mcs_co_t0/thorn_Mxco;
thorn_Ns_co = thorn_Mns_co_t0/thorn_Mxco;
thorn_Ps_co = thorn_Mps_co_t0/thorn_Mxco;
thorn_Ws_co = thorn_Mws_co_t0/thorn_Mxco;

thorn_Cs_no = thorn_Mcs_no_t0/thorn_Mxno;
thorn_Ns_no = thorn_Mns_no_t0/thorn_Mxno;
thorn_Ps_no = thorn_Mps_no_t0/thorn_Mxno;
thorn_Ws_no = thorn_Mws_no_t0/thorn_Mxno;

thorn_Cs_po = thorn_Mcs_po_t0/thorn_Mxpo;
thorn_Ns_po = thorn_Mns_po_t0/thorn_Mxpo;
thorn_Ps_po = thorn_Mps_po_t0/thorn_Mxpo;
thorn_Ws_po = thorn_Mws_po_t0/thorn_Mxpo;

thorn_Cs_wo = thorn_Mcs_wo_t0/thorn_Mxwo;
thorn_Ns_wo = thorn_Mns_wo_t0/thorn_Mxwo;
thorn_Ps_wo = thorn_Mps_wo_t0/thorn_Mxwo;
thorn_Ws_wo = thorn_Mws_wo_t0/thorn_Mxwo;

## AUX VARIABLE initial values for fractions of structural dry matter in shoot, root, mycor
thorn_Fxco = thorn_Mxco_t0/thorn_Mx_t0;
thorn_Fxwo = thorn_Mxwo_t0/thorn_Mx_t0;
thorn_Fxno = thorn_Mxno_t0/thorn_Mx_t0;
thorn_Fxpo = thorn_Mxpo_t0/thorn_Mx_t0;


#R only make a matrix to store the variables for plotting
#n.steps<-360*10
n.steps<-1000
M<-matrix(0,nrow=n.steps,ncol=41)

wrapper_fn_mod <- function(uptake)
{
  ## the for loop
  for(i in 1:n.steps) {
    
  uptake_change <- uptake

  base =  0.0002*100; #aDGVM produces numbers around 0.00001 to 0.001 for C_net/plant_mass
  thorn_Kc  = base*1.0;
  thorn_Kw  = 0.0015;
  if(uptake_change == 1)
  {
    thorn_Kn  = 0.0001;
    # thorn_Kn  = 0.05 - pmin(0.001,(i*0.1));
    print("Low N")
    print(thorn_Kn)
  }
  if(uptake_change == 0)
  {
    thorn_Kn  = 0.25;
    print("High N")
    print(thorn_Kn)
  }
  
  thorn_Kp  = 0.1;
  
  thorn_Mx = thorn_Mxco + thorn_Mxwo + thorn_Mxno + thorn_Mxpo ; #3
  
  ##---------------------------------------------------------------------------------------------
  ## adding substrate transport between pools following Thornley (1998). Substrates will be transported
  ## between plants compartments based on concentration differences and resistances. 
  ## This avoids the issue of reduced allocation as this model does not allocate growth, growth of each compartment is 
  ## indipenaent of the others but it allows every compartment to be both a source and a sink. Should an individual be top-killed
  ## i.e. loss of the carbon organ and substrates contained within, C, N, P and W will be transported from all other organs until 
  ## there is no concentration difference. This increases carbon organ growth and decreases growth in all other compartments. 
  
  # these are specific transport resistances for substrates
  # I'm assuming they are equal for all substrates and transport directions. 
  p_co = p_no = p_po = p_wo =1; # eq.4 Thornley 1998 
  
  q = 1 # eq.4 scaling parameter "which, presumably, is a function of plant architecture" (Thornley 1998)
  
  # Transport resistance of each compartment 
  res_co = p_co / (thorn_Mxco)^q; # eq.4 Thornley 1998 
  res_no = p_no / (thorn_Mxno)^q; # eq.4 Thornley 1998 
  res_po = p_po / (thorn_Mxpo)^q; # eq.4 Thornley 1998 
  res_wo = p_wo / (thorn_Mxwo)^q; # eq.4 Thornley 1998 
  
  # Transport of substrates between compartments. Here I extend Thornley uni-directional model to 
  # make it bi-directional. 
  # NOTE: this is not complete - I need to include transport between other compartments i.e. Po to Wo, Po to No, Wo to No for C,N,P & W.
  
  # carbon transport
  Tc_co_to_no = 0.2*(thorn_Cs_co - thorn_Cs_no) / (res_co + res_no) # transport of carbon substrate from the carbon organ to the nitrogen organ
  Tc_co_to_po = 0.2*(thorn_Cs_co - thorn_Cs_po) / (res_co + res_po) # transport of carbon substrate from the carbon organ to the phosphorus organ
  Tc_co_to_wo = 0.2*(thorn_Cs_co - thorn_Cs_wo) / (res_co + res_wo) # transport of carbon substrate from the carbon organ to the water organ
  
  Tc_no_to_po = 0.2*(thorn_Cs_no - thorn_Cs_po) / (res_no + res_po) # transport of carbon substrate from the nitrogen organ to the phosphorus organ
  Tc_no_to_wo = 0.2*(thorn_Cs_no - thorn_Cs_wo) / (res_no + res_wo) # transport of carbon substrate from the nitrogen organ to the water organ
  Tc_po_to_wo = 0.2*(thorn_Cs_po - thorn_Cs_wo) / (res_po + res_wo) # transport of carbon substrate from the phosphorus organ to the water organ

  print(c("Tc_cn Tc_cp Tc_cw Tc_np Tc_nw Tc_pw"))
  print(round(c(Tc_co_to_no, Tc_co_to_po, Tc_co_to_wo, Tc_no_to_po, Tc_no_to_wo, Tc_po_to_wo),2))
  
  # nitrogen transport
  Tn_co_to_no = 0.2*(thorn_Ns_co - thorn_Ns_no) / (res_co + res_no) # transport of nitrogen substrate from the carbon organ to the nitrogen organ
  Tn_co_to_po = 0.2*(thorn_Ns_co - thorn_Ns_po) / (res_co + res_po) # transport of nitrogen substrate from the carbon organ to the phosphorus organ
  Tn_co_to_wo = 0.2*(thorn_Ns_co - thorn_Ns_wo) / (res_co + res_wo) # transport of nitrogen substrate from the carbon organ to the water organ
  
  Tn_no_to_po = 0.2*(thorn_Ns_no - thorn_Ns_po) / (res_no + res_po) # transport of nitrogen substrate from the nitrogen organ to the phosphorus organ
  Tn_no_to_wo = 0.2*(thorn_Ns_no - thorn_Ns_wo) / (res_no + res_wo) # transport of nitrogen substrate from the nitrogen organ to the water organ
  Tn_po_to_wo = 0.2*(thorn_Ns_po - thorn_Ns_wo) / (res_po + res_wo) # transport of nitrogen substrate from the phosphorus organ to the water organ
  # phosphorus transport
  Tp_co_to_no = 0.2*(thorn_Ps_co - thorn_Ps_no) / (res_co + res_no) # transport of phosphorus substrate from the carbon organ to the nitrogen organ
  Tp_co_to_po = 0.2*(thorn_Ps_co - thorn_Ps_po) / (res_co + res_po) # transport of phosphorus substrate from the carbon organ to the phosphorus organ
  Tp_co_to_wo = 0.2*(thorn_Ps_co - thorn_Ps_wo) / (res_co + res_wo) # transport of phosphorus substrate from the carbon organ to the water organ
  
  Tp_no_to_po = 0.2*(thorn_Ps_no - thorn_Ps_po) / (res_no + res_po) # transport of phosphorus substrate from the nitrogen organ to the phosphorus organ
  Tp_no_to_wo = 0.2*(thorn_Ps_no - thorn_Ps_wo) / (res_no + res_wo) # transport of phosphorus substrate from the nitrogen organ to the water organ
  Tp_po_to_wo = 0.2*(thorn_Ps_po - thorn_Ps_wo) / (res_po + res_wo) # transport of phosphorus substrate from the phosphorus organ to the water organ
  # water transport
  Tw_co_to_no = 0.2*(thorn_Ws_co - thorn_Ws_no) / (res_co + res_no) # transport of water substrate from the carbon organ to the nitrogen organ
  Tw_co_to_po = 0.2*(thorn_Ws_co - thorn_Ws_po) / (res_co + res_po) # transport of water substrate from the carbon organ to the phosphorus organ
  Tw_co_to_wo = 0.2*(thorn_Ws_co - thorn_Ws_wo) / (res_co + res_wo) # transport of water substrate from the carbon organ to the water organ
  
  Tw_no_to_po = 0.2*(thorn_Ws_no - thorn_Ws_po) / (res_no + res_po) # transport of water substrate from the carbon organ to the nitrogen organ
  Tw_no_to_wo = 0.2*(thorn_Ws_no - thorn_Ws_wo) / (res_no + res_wo) # transport of water substrate from the carbon organ to the phosphorus organ
  Tw_po_to_wo = 0.2*(thorn_Ws_po - thorn_Ws_wo) / (res_po + res_wo) # transport of water substrate from the carbon organ to the water organ
  ##---------------------------------------------------------------------------------------------
  # print(round(c(thorn_lambda_sh,thorn_lambda_rt,thorn_lambda_my,thorn_lambda_rw),2))
  # print(sum(c(thorn_lambda_sh,thorn_lambda_rt,thorn_lambda_my,thorn_lambda_rw)))
  ## do I need to redefine this for each compartment? yes yes I do... no no I dont
  thorn_Uc = thorn_Kc * thorn_Mxco;
  thorn_Un = thorn_Kn * thorn_Mxno;
  thorn_Up = thorn_Kp * thorn_Mxpo;
  thorn_Uw = thorn_Kw * thorn_Mxwo;
  
  thorn_Mcs_co = max(0.0001, thorn_Mcs_co + thorn_Uc);
  # thorn_Mns_co = max(0.0001, thorn_Mns_co - thorn_Fnx * thorn_G_co );
  # thorn_Mps_co = max(0.0001, thorn_Mps_co - thorn_Fpx * thorn_G_co );
  # thorn_Mws_co = max(0.0001, thorn_Mws_co - thorn_Fwx * thorn_G_co );
  
  # thorn_Mcs_no = max(0.0001, thorn_Mcs_no - thorn_Fcx * thorn_G_no );
  thorn_Mns_no = max(0.0001, thorn_Mns_no + thorn_Un );
  # thorn_Mps_no = max(0.0001, thorn_Mps_no - thorn_Fpx * thorn_G_no );
  # thorn_Mws_no = max(0.0001, thorn_Mws_no - thorn_Fwx * thorn_G_no );
  
  # thorn_Mcs_po = max(0.0001, thorn_Mcs_po - thorn_Fcx * thorn_G_po );
  # thorn_Mns_po = max(0.0001, thorn_Mns_po - thorn_Fnx * thorn_G_po );
  thorn_Mps_po = max(0.0001, thorn_Mps_po + thorn_Up );
  # thorn_Mws_po = max(0.0001, thorn_Mws_po - thorn_Fwx * thorn_G_po );
  
  # thorn_Mcs_wo = max(0.0001, thorn_Mcs_wo - thorn_Fcx * thorn_G_wo );
  # thorn_Mns_wo = max(0.0001, thorn_Mns_wo - thorn_Fnx * thorn_G_wo );
  # thorn_Mps_wo = max(0.0001, thorn_Mps_wo - thorn_Fpx * thorn_G_wo );
  thorn_Mws_wo = max(0.0001, thorn_Mws_wo + thorn_Uw );
  
  thorn_Cs_co = thorn_Mcs_co/thorn_Mxco; #2
  thorn_Ns_co = thorn_Mns_co/thorn_Mxco; #2
  thorn_Ps_co = thorn_Mps_co/thorn_Mxco; #2
  thorn_Ws_co = thorn_Mws_co/thorn_Mxco; #2
  
  thorn_Cs_no = thorn_Mcs_no/thorn_Mxno; #2
  thorn_Ns_no = thorn_Mns_no/thorn_Mxno; #2
  thorn_Ps_no = thorn_Mps_no/thorn_Mxno; #2
  thorn_Ws_no = thorn_Mws_no/thorn_Mxno; #2
  
  thorn_Cs_po = thorn_Mcs_po/thorn_Mxpo; #2
  thorn_Ns_po = thorn_Mns_po/thorn_Mxpo; #2
  thorn_Ps_po = thorn_Mps_po/thorn_Mxpo; #2
  thorn_Ws_po = thorn_Mws_po/thorn_Mxpo; #2
  
  thorn_Cs_wo = thorn_Mcs_wo/thorn_Mxwo; #2
  thorn_Ns_wo = thorn_Mns_wo/thorn_Mxwo; #2
  thorn_Ps_wo = thorn_Mps_wo/thorn_Mxwo; #2
  thorn_Ws_wo = thorn_Mws_wo/thorn_Mxwo; #2
  
  ## make sure thorn_G is the right size. you can get this right through thorn_Kg which is a pure calibration parameter, 
  ## but also by making sure the substrate cocentrations are reasonable
  thorn_G_co = thorn_Kg*thorn_Mxco*thorn_Cs_co*thorn_Ns_co*thorn_Ps_co*thorn_Ws_co ; # see 13 and 12
  thorn_G_no = thorn_Kg*thorn_Mxno*thorn_Cs_no*thorn_Ns_no*thorn_Ps_no*thorn_Ws_no ; # see 13 and 12
  thorn_G_po = thorn_Kg*thorn_Mxpo*thorn_Cs_po*thorn_Ns_po*thorn_Ps_po*thorn_Ws_po ; # see 13 and 12
  thorn_G_wo = thorn_Kg*thorn_Mxwo*thorn_Cs_wo*thorn_Ns_wo*thorn_Ps_wo*thorn_Ws_wo ; # see 13 and 12

  thorn_Mcs_co = max(0.0001, thorn_Mcs_co - thorn_Fcx * thorn_G_co - Tc_co_to_no - Tc_co_to_po - Tc_co_to_wo);
  thorn_Mns_co = max(0.0001, thorn_Mns_co - thorn_Fnx * thorn_G_co - Tn_co_to_no - Tn_co_to_po - Tn_co_to_wo);
  thorn_Mps_co = max(0.0001, thorn_Mps_co - thorn_Fpx * thorn_G_co - Tp_co_to_no - Tp_co_to_po - Tp_co_to_wo);
  thorn_Mws_co = max(0.0001, thorn_Mws_co - thorn_Fwx * thorn_G_co - Tw_co_to_no - Tw_co_to_po - Tw_co_to_wo);
  
  thorn_Mcs_no = max(0.0001, thorn_Mcs_no - thorn_Fcx * thorn_G_no - Tc_no_to_po - Tc_no_to_wo + Tc_co_to_no);
  thorn_Mns_no = max(0.0001, thorn_Mns_no - thorn_Fnx * thorn_G_no + Tn_co_to_no - Tn_no_to_po - Tn_no_to_wo);
  thorn_Mps_no = max(0.0001, thorn_Mps_no - thorn_Fpx * thorn_G_no - Tp_no_to_po - Tp_no_to_wo + Tp_co_to_no);
  thorn_Mws_no = max(0.0001, thorn_Mws_no - thorn_Fwx * thorn_G_no - Tw_no_to_po - Tw_no_to_wo + Tw_co_to_no);
  
  thorn_Mcs_po = max(0.0001, thorn_Mcs_po - thorn_Fcx * thorn_G_po - Tc_po_to_wo + Tc_no_to_po + Tc_co_to_po);
  thorn_Mns_po = max(0.0001, thorn_Mns_po - thorn_Fnx * thorn_G_po - Tn_po_to_wo + Tn_no_to_po + Tn_co_to_po);
  thorn_Mps_po = max(0.0001, thorn_Mps_po - thorn_Fnx * thorn_G_po - Tn_po_to_wo + Tn_no_to_po + Tn_co_to_po);
  thorn_Mws_po = max(0.0001, thorn_Mws_po - thorn_Fwx * thorn_G_po - Tw_po_to_wo + Tw_no_to_po + Tw_co_to_po);
  
  thorn_Mcs_wo = max(0.0001, thorn_Mcs_wo - thorn_Fcx * thorn_G_wo + Tc_co_to_wo + Tc_no_to_wo + Tc_po_to_wo);
  thorn_Mns_wo = max(0.0001, thorn_Mns_wo - thorn_Fnx * thorn_G_wo + Tn_co_to_wo + Tn_no_to_wo + Tn_po_to_wo);
  thorn_Mps_wo = max(0.0001, thorn_Mps_wo - thorn_Fpx * thorn_G_wo + Tp_co_to_wo + Tp_no_to_wo + Tp_po_to_wo);
  thorn_Mws_wo = max(0.0001, thorn_Mws_wo - thorn_Fwx * thorn_G_wo + Tw_co_to_wo + Tw_no_to_wo + Tw_po_to_wo);
  
  # thorn_Mcs = max(0, thorn_Mcs + thorn_Uc - thorn_Fcx * thorn_G); # 12
  # thorn_Mns = max(0, thorn_Mns + thorn_Un - thorn_Fnx * thorn_G); # 12
  # thorn_Mps = max(0, thorn_Mps + thorn_Up - thorn_Fpx * thorn_G); # 12
  # thorn_Mws = max(0, thorn_Mws + thorn_Uw - thorn_Fwx * thorn_G); # 12
  
  thorn_Mxco = max(0.0001, thorn_Mxco + thorn_G_co) ;# 13
  thorn_Mxno = max(0.0001, thorn_Mxno + thorn_G_no) ;# 13
  thorn_Mxpo = max(0.0001, thorn_Mxpo + thorn_G_po) ;# 13
  thorn_Mxwo = max(0.0001, thorn_Mxwo + thorn_G_wo) ;# 13
  
  thorn_Mx = thorn_Mxco + thorn_Mxwo + thorn_Mxno + thorn_Mxpo ; #3
  
  # thorn_Uc = (thorn_Kc * thorn_Mxco) / (1 + thorn_Cs/thorn_Jcs)
  # thorn_Uw = (thorn_Kw * thorn_Mxwo) / (1 + thorn_Ws/thorn_Jws)
  # thorn_Un = (thorn_Kn * thorn_Mxno) / (1 + thorn_Ns/thorn_Jns)
  # thorn_Up = (thorn_Kp * thorn_Mxpo) / (1 + thorn_Ps/thorn_Jps)
  
  #use this code to simulate a loss in shoots e.g in fire
  # if(i == 100)
  # {
  # 	thorn_Mxco = thorn_Mxco*0.1;
  # 	thorn_Mcs_co = thorn_Mcs_co*0.0;
  # 	thorn_Mns_co = thorn_Mns_co*0.0;
  #   thorn_Mps_co = thorn_Mps_co*0.0;
  # 	thorn_Mws_co = thorn_Mws_co*0.0;
  # }

  thorn_Cs_co = thorn_Mcs_co/thorn_Mxco; #2
  thorn_Ns_co = thorn_Mns_co/thorn_Mxco; #2
  thorn_Ps_co = thorn_Mps_co/thorn_Mxco; #2
  thorn_Ws_co = thorn_Mws_co/thorn_Mxco; #2
  
  thorn_Cs_no = thorn_Mcs_no/thorn_Mxno; #2
  thorn_Ns_no = thorn_Mns_no/thorn_Mxno; #2
  thorn_Ps_no = thorn_Mps_no/thorn_Mxno; #2
  thorn_Ws_no = thorn_Mws_no/thorn_Mxno; #2
  
  thorn_Cs_po = thorn_Mcs_po/thorn_Mxpo; #2
  thorn_Ns_po = thorn_Mns_po/thorn_Mxpo; #2
  thorn_Ps_po = thorn_Mps_po/thorn_Mxpo; #2
  thorn_Ws_po = thorn_Mws_po/thorn_Mxpo; #2
  
  thorn_Cs_wo = thorn_Mcs_wo/thorn_Mxwo; #2
  thorn_Ns_wo = thorn_Mns_wo/thorn_Mxwo; #2
  thorn_Ps_wo = thorn_Mps_wo/thorn_Mxwo; #2
  thorn_Ws_wo = thorn_Mws_wo/thorn_Mxwo; #2
  
  thorn_Fxco = thorn_Mxco/thorn_Mx; #4
  thorn_Fxwo = thorn_Mxwo/thorn_Mx; #4
  thorn_Fxno = thorn_Mxno/thorn_Mx; #4
  thorn_Fxpo = thorn_Mxpo/thorn_Mx; #4
  
  M[i,1] <<- thorn_Mxco
  M[i,2] <<- thorn_Mxwo
  M[i,3] <<- thorn_Mxno
  M[i,4] <<- thorn_Mxpo
  
  M[i,5] <<- thorn_Fxco
  M[i,6] <<- thorn_Fxwo
  M[i,7] <<- thorn_Fxno
  M[i,8] <<- thorn_Fxpo
  
  M[i,9]  <<- thorn_Mcs_co
  M[i,10] <<- thorn_Mws_co
  M[i,11] <<- thorn_Mns_co
  M[i,12] <<- thorn_Mps_co
  
  M[i,13] <<- thorn_Mcs_no
  M[i,14] <<- thorn_Mws_no
  M[i,15] <<- thorn_Mns_no
  M[i,16] <<- thorn_Mps_no

  M[i,17] <<- thorn_Mcs_po
  M[i,18] <<- thorn_Mws_po
  M[i,19] <<- thorn_Mns_po
  M[i,20] <<- thorn_Mps_po

  M[i,21] <<- thorn_Mcs_wo
  M[i,22] <<- thorn_Mws_wo
  M[i,23] <<- thorn_Mns_wo
  M[i,24] <<- thorn_Mps_wo
  
  M[i,25] <<- thorn_Cs_co
  M[i,26] <<- thorn_Ws_co
  M[i,27] <<- thorn_Ns_co
  M[i,28] <<- thorn_Ps_co
  
  M[i,29] <<- thorn_Cs_co/thorn_Ns_co
  M[i,30] <<- thorn_Cs_co/thorn_Ps_co
  M[i,31] <<- thorn_Ns_co/thorn_Ps_co
  
  M[i,32] <<- Tc_co_to_no
  M[i,33] <<- Tc_co_to_po
  M[i,34] <<- Tc_co_to_wo
  M[i,35] <<- Tc_no_to_po
  M[i,36] <<- Tc_no_to_wo
  M[i,37] <<- Tc_po_to_wo
  
  M[i,38] <<- thorn_G_co
  M[i,39] <<- thorn_G_no
  M[i,40] <<- thorn_G_po
  M[i,41] <<- thorn_G_wo
  
  }
}
colnames(M)<-c(
   "Mxco"
  ,"Mxwo"
  ,"Mxno"
  ,"Mxpo"
  ,"Fxco"
  ,"Fxwo"
  ,"Fxno"
  ,"Fxpo"
  ,"thorn_Mcs_co"
  ,"thorn_Mws_co"
  ,"thorn_Mns_co"
  ,"thorn_Mps_co"
  ,"thorn_Mcs_no"
  ,"thorn_Mws_no"
  ,"thorn_Mns_no"
  ,"thorn_Mps_no"
  ,"thorn_Mcs_po"
  ,"thorn_Mws_po"
  ,"thorn_Mns_po"
  ,"thorn_Mps_po"
  ,"thorn_Mcs_wo"
  ,"thorn_Mws_wo"
  ,"thorn_Mns_wo"
  ,"thorn_Mps_wo"
  ,"Cs_co"
  ,"Ws_co"
  ,"Ns_co"
  ,"Ps_co"
  ,"C:N_co"
  ,"C:p_co"
  ,"N:P_co"
  ,"Tc_co_to_no"
  ,"Tc_co_to_po"
  ,"Tc_co_to_wo"
  ,"Tc_no_to_po"
  ,"Tc_no_to_wo"
  ,"Tc_po_to_wo"
  ,"thorn_G_co"
  ,"thorn_G_no"
  ,"thorn_G_po"
  ,"thorn_G_wo")



wrapper_fn_mod(0) 
M <<- M

graphics.off()
pdf( file="TAM_removing_CO_source_sink.pdf", height=6, width=8 )
par( mar=c(4,4,1,1) )
#dev.new(width=15,height=7)
par(mfcol=c(2,2))

matplot(log(M[,1:4]),type="l",lty=c(rep(1,4)),col=mycol,lwd=2)
legend("topleft",col=mycol[c(1:4)],lty=c(rep(1,4)),legend=colnames(M)[c(1:4)],cex=1.0)

matplot(M[,5:8],type="l",lty=c(rep(1,4)),col=mycol,lwd=2,add=F)
legend("topright",col=mycol[c(1:4,1:4)],lty=c(rep(1,4),rep(2,4)),legend=colnames(M)[5:8],cex=1.0)

matplot((M[,32:34]),type="l",lty=c(rep(1,4)),col=mycol[1:4],lwd=2,add=F)
abline(h=0, lty=2, lwd=0.5)
legend("topleft",col=mycol[1:4],lty=c(rep(1,4)),legend=colnames(M)[c(32:34)],cex=1.0)

# matplot((M[,38:41]),type="l",lty=c(rep(1,4)),col=mycol[1:4],lwd=2,add=F)
# legend("topleft",col=mycol[1:4],lty=c(rep(1,4)),legend=colnames(M)[c(38:41)],cex=1.0)

matplot(M[,38]/(M[,38]+M[,39]+M[,40]+M[,41]),type="l",lty=c(rep(1)),col=mycol[1],lwd=2,add=F, ylim=c(0,1))
matplot(M[,41]/(M[,38]+M[,39]+M[,40]+M[,41]),type="l",lty=c(rep(1)),col=mycol[2],lwd=2,add=T, ylim=c(0,1))
matplot(M[,40]/(M[,38]+M[,39]+M[,40]+M[,41]),type="l",lty=c(rep(1)),col=mycol[3],lwd=2,add=T, ylim=c(0,1))
matplot(M[,41]/(M[,38]+M[,39]+M[,40]+M[,41]),type="l",lty=c(rep(1)),col=mycol[4],lwd=2,add=T, ylim=c(0,1))
legend("topright",col=mycol[1:4],lty=c(rep(1)),legend=c("Alloc CO","Alloc WO","Alloc NO","Alloc PO"),cex=1.0)

graphics.off()


graphics.off()
pdf( file="TAM_removing_CO_source_sink_compare_allocation.pdf", height=6, width=8 )
par( mar=c(4,4,1,1) )
#dev.new(width=15,height=7)
par(mfcol=c(1,2))

matplot(M[,38]/(M[,38]+M[,39]+M[,40]+M[,41]),type="l",lty=c(rep(1)),col=mycol[1],lwd=2,add=F, ylim=c(0,1), main="High Nitrogen uptake")
matplot(M[,41]/(M[,38]+M[,39]+M[,40]+M[,41]),type="l",lty=c(rep(2)),col=mycol[2],lwd=2,add=T, ylim=c(0,1))
matplot(M[,39]/(M[,38]+M[,39]+M[,40]+M[,41]),type="l",lty=c(rep(3)),col=mycol[3],lwd=2,add=T, ylim=c(0,1))
matplot(M[,40]/(M[,38]+M[,39]+M[,40]+M[,41]),type="l",lty=c(rep(4)),col=mycol[4],lwd=2,add=T, ylim=c(0,1))
legend("topright",col=mycol[1:4],lty=c(1,2,3,4),legend=c("Alloc CO","Alloc WO","Alloc NO","Alloc PO"),cex=1.0)

wrapper_fn_mod(1) 
M1 <<- M

matplot(M1[,38]/(M1[,38]+M1[,39]+M1[,40]+M1[,41]),type="l",lty=c(rep(1)),col=mycol[1],lwd=2,add=F, ylim=c(0,1), main="Low Nitrogen uptake")
matplot(M1[,41]/(M1[,38]+M1[,39]+M1[,40]+M1[,41]),type="l",lty=c(rep(2)),col=mycol[2],lwd=2,add=T, ylim=c(0,1))
matplot(M1[,39]/(M1[,38]+M1[,39]+M1[,40]+M1[,41]),type="l",lty=c(rep(3)),col=mycol[3],lwd=2,add=T, ylim=c(0,1))
matplot(M1[,40]/(M1[,38]+M1[,39]+M1[,40]+M1[,41]),type="l",lty=c(rep(4)),col=mycol[4],lwd=2,add=T, ylim=c(0,1))
legend("topright",col=mycol[1:4],lty=c(1,2,3,4),legend=c("Alloc CO","Alloc WO","Alloc NO","Alloc PO"),cex=1.0)

graphics.off()

graphics.off()
pdf( file="TAM_removing_CO_source_sink_substrate_concentrations.pdf", height=6, width=8 )
par( mar=c(4,4,1,1) )
#dev.new(width=15,height=7)
par(mfcol=c(2,2))

matplot(M[,c(9, 13, 17, 21)],type="l",lty=c(rep(1,4)),col=mycol,lwd=2,add=F)
legend("topleft",col=mycol[c(1:4)],lty=c(rep(1,4)),legend=colnames(M)[c(9, 13, 17, 21)],cex=1.0)

matplot(M[,c(10, 14, 18, 22)],type="l",lty=c(rep(1,4)),col=mycol,lwd=2,add=F)
legend("topleft",col=mycol[c(1:4)],lty=c(rep(1,4)),legend=colnames(M)[c(10, 14, 18, 22)],cex=1.0)

matplot((M[,c(11, 15, 19, 23)]),type="l",lty=c(rep(1,4)),col=mycol,lwd=2,add=F)
legend("topleft",col=mycol,lty=c(rep(1,4)),legend=colnames(M)[c(11, 15, 19, 23)],cex=1.0)

matplot((M[,c(12, 16, 20, 24)]),type="l",lty=c(rep(1,4)),col=mycol,lwd=2,add=F)
legend("topleft",col=mycol,lty=c(rep(1,4)),legend=colnames(M)[c(12, 16, 20, 24)],cex=1.0)

graphics.off()


print(tail(M))

# 
# evapotr_ = 0 #53629576106646967e-06
# stom_cond_tmp = -1.0656466997716052e-12
# boul_cond_tmp = 0.010730861431038661
# hum = 0.6
# eS = 3.6
# crown_area_0_ = 0.2
# lai_ = 0.01
# 
# leaf_helper =  log( (evapotr_/(crown_area_0_*lai_) *(1./(stom_cond_tmp*1000.)+1./(boul_cond_tmp*1000.) ) + hum*eS)/0.6108 );
# leaf_temp_ = 237.3*leaf_helper/(17.27-leaf_helper);
# 
# print(c(leaf_helper,leaf_temp_))


# OK I think I have it sorted now - at least in the R code I now have TAM working with a third substrate pool = water. Now roots take up water, C and P. The model behaves qualitatively as before. Now however one has to be a bit more careful about parameters - there is simply a smaller area of parameter space where the model "works". This is something that will come to bite us when we move to having uptake rates of N and P being a fxn of soil N and P.
# 
# In the R code I now have a thorn_Kw parameter which is essentially the uptake rate of water by root biomass. We (you) need to figure out how to multiply this by some water availability variable. If you can supply me with a a variable on a zero to one scale that you like (it should be something that inspires plants to allocate more to roots). Also remember that we already have water limiting As. So what we really want here is an indicator of a "need" to allocate to roots. Note that in TAM if water limits As, it limits carbon uptake which should inspire more shoots. So we might want to make the TAM's As not water limited. Water limitation will naturally come into play though since the water concentration in TAM will limit the growth parameter.
#  
# Anyhow sorry about the dense text. What I need for now is the "water availability factor"

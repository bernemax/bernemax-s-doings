option profile = 1;
option profiletol = 0.01;

set
*time set
t /t1*t24/
*generator set
g /g1*g12/
*line set
l /l1*l68/
*node set
n /n1*n24/
*expansion Korridor
k /k1*k4/

*referenze node
ref(n) /n3/
exist(l) /l1*l34/
prosp(l) /l35*l68/
Map_send(l,n)               mapping of sending-line to node
Map_res(l,n)                mapping of resiving-line to node
Map_gen(g,n)                mapping of the generation to node
;

scalars
Sbase /100/
M    /1000/
ILmax /100000/
;
Parameter
genup                       upload table for generation data\parameter like costs\startups and co.
lineup                      upload table for line data

demand(n,t)

P_max(g)
P_min(g)
P_init(g)
ramp_up(g)
ramp_down(g)

ramp_up_reserve(g)             Maximum up reserve capacity of generating unit
ramp_down_reserve(g)           Maximum down reserve capacity of generating unit

gen_costs(g)
su_costs(g)

line_cap(l)                 line capacity
b(l)                        line susceptance = 1\line reactance

Inv_costs(l)
price(n,t)
;

table System_demand (t,*)  Total system demand for each t

         load
t1       1775.835
t2       1669.815
t3       1590.3
t4       1563.795
t5       1563.795
t6       1590.3
t7       1961.37
t8       2279.43
t9       2517.975
t10      2544.48
t11      2544.48
t12      2517.975
t13      2517.975
t14      2517.975
t15      2464.965
t16      2464.965
t17      2623.995
t18      2650.5
t19      2650.5
t20      2544.48
t21      2411.955
t22      2199.915
t23      1934.865
t24      1669.815
;

table Load_share (n,*) Load distribution of the Total system demand  
         share
n1       0.038
n2       0.034
n3       0.063
n4       0.026
n5       0.025
n6       0.048
n7       0.044
n8       0.06
n9       0.061
n10      0.068
n11      0
n12      0
n13      0.093
n14      0.068
n15      0.111
n16      0.035
n17      0
n18      0.117
n19      0.064
n20      0.045
n21      0
n22      0
n23      0
n24      0
;
*########################################################set & parameter loading####################################################
$onecho > IEEE.txt
set=Map_gen             rng=Mapping!A2:B13                rdim=2 cDim=0
set=Map_send            rng=Mapping!D2:E69                rdim=2 cDim=0    
set=Map_res             rng=Mapping!G2:H69                rdim=2 cDim=0
            
par=genup               rng=Generation!A1:R13             rdim=1 cDim=1
par=lineup              rng=lines!A2:D70                  rdim=1 cDim=1

$offecho

$onUNDF
$call   gdxxrw Data_IEEE_24.xlsx @IEEE.txt
$GDXin  Data_IEEE_24.gdx
$load   Map_gen, Map_send, Map_res
$load   genup, lineup
$offUNDF
;
**********************************************************parameter assignment*******************************************************
P_max(g)            =   genup(g,'Pi_max')
;
P_min(g)            =   genup(g,'Pi_min')
;
P_init(g)           =   genup(g,'Pi_init')
;
ramp_up(g)          =   genup(g,'Ri_up')
;
ramp_down(g)        =   genup(g,'Ri_down')
;
ramp_up_reserve(g)  =   genup(g,'Ri+')
;             
ramp_down_reserve(g)=   genup(g,'Ri-')
;
gen_costs(g)        =   genup(g,'Ci')
;
su_costs(g)         =   genup(g,'Ci_su')
;
demand(n,t)         =   system_demand(t,'load')*load_share(n,'share')+EPS
;
line_cap(l)         =   lineup(l,'cap')
;   
b(l)                =   lineup(l,'b')
;
Inv_costs(l)        =   lineup(l,'Inv_costs')/10
;

*execute_unload "check.gdx";
*$stop
*############################################################variables########################################################
Variables
Costs
Power_flow(l,t)
Theta(n,t)
Su(g,t)

;

Positive variables
gen(g,t)
P_on(g,t)
Load_shed (n,t)
X_dem(n,t)
LS(n,t)
;
Integer variable
x(l,t)
;
*#############################################################Equations#######################################################
Equations
Total_costs
Line_investment
Balance

max_gen
max_cap
min_cap
startup_constr
Ramp_up_constr
Ramp_down_constr
P_on_start


Ex_line_angle
Ex_Line_neg_flow
Ex_line_pos_flow
Prosp_line_neg_flow
Prosp_line_pos_flow
Linearization_prosp_line_neg
Linearization_prosp_line_pos

*LS_det
Theta_LB
Theta_UB
Theta_ref
;
*#################################################################Objective function#############################################

Total_costs..           costs =e= sum((g,t),gen(g,t) * gen_costs(g))
                                + sum((g,t), Su(g,t) * su_costs(g))
                                + sum((n,t), LS(n,t) * 3000)
                                + sum((l,t),Inv_costs(l) * x(l,t))
;
*##################################################################Energy balance################################################

Balance(n,t)..          demand(n,t) - LS (n,t) =e= sum(g$Map_gen(g,n), gen(g,t))
                                                 + sum(l$Map_res(l,n),power_flow(l,t))
                                                 - sum(l$Map_send(l,n),power_flow(l,t))
;
*##################################################################Investment budget#############################################

Line_investment..            sum((t,l)$prosp(l),Inv_costs(l)*x(l,t)) =l= ILmax
;
*##################################################################Generation funcioning##########################################

max_gen(g,t)..                                Gen(g,t)  =l= P_on(g,t)
;
max_cap(g,t)..                                P_on(g,t) =l= P_max(g) 
;
min_cap(g,t)$(ord(t) gt 1)..                  P_on(g,t)  =g=  P_min(g)
;
startup_constr(g,t)..                         P_on(g,t) - P_on(g,t-1) =l= Su(g,t)
;
Ramp_up_constr(g,t)$(ord(t) gt 1)..           Su(g,t) =l= ramp_up(g)
;
Ramp_down_constr(g,t)$(ord(t) gt 1)..         Su(g,t) =g= -ramp_down(g)
;
P_on_start(g,t)$(ord(t) = 1)..                P_on(g,t) =e= P_init(g)
;

*###############################################################Grid technical functioning#########################################

Ex_line_angle(l,t)$exist(l)..                    power_flow(l,t) =e=  (B(l)*(sum(n$Map_send(l,n), Theta(n,t))-sum(n$Map_res(l,n), Theta(n,t)))) * sbase
;
Ex_line_neg_flow(l,t)$exist(l)..                 power_flow(l,t) =g= -Line_cap(l)
;
Ex_line_pos_flow(l,t)$exist(l)..                 power_flow(l,t) =l=  Line_cap(l)
;

Prosp_line_neg_flow(l,t)$prosp(l)..              power_flow(l,t) =g= -x(l,t) * Line_cap(l)  
;
Prosp_line_pos_flow(l,t)$prosp(l)..              power_flow(l,t) =l=  x(l,t) * Line_cap(l)
;
Linearization_prosp_line_neg(l,t)$prosp(l)..     -(1-x(l,t))*M   =l= power_flow(l,t) - B(l) * (sum(n$Map_send(l,n),Theta(n,t))-sum(n$Map_res(l,n),Theta(n,t)))
;
Linearization_prosp_line_pos(l,t)$prosp(l)..     (1-x(l,t))*M    =g= power_flow(l,t) - B(l) * (sum(n$Map_send(l,n),Theta(n,t))-sum(n$Map_res(l,n),Theta(n,t)))
;

Theta_LB(n,t)..                                  -3.1415         =l= Theta(n,t)
;
Theta_UB(n,t)..                                  3.1415          =g= Theta(n,t)
;
Theta_ref(n,t)..                                 Theta(n,t)$ref(n) =l= 0
;
*#############################################################solving##############################################################

Model TEP_IEEE_24 /all/;
*option limrow = 1e9;
TEP_IEEE_24.scaleopt = 1;

solve TEP_IEEE_24 using MIP minimizing costs;

price(n,t) = Balance.m(n,t)*(-1);

execute_unload "check.gdx" 
;





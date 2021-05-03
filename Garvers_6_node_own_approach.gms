Sets

n /n1*n6/
g /g1*g3/

k /k1*k3/
*expansion korridor


MapG (g,n)  /g1.n1,g2.n3,g3.n6/
ref(n)      /n1/
;
alias (n,bus)
;
$setglobal NO_LS_Invest "*"  if "*" objectiv function is to minimize investment-costs, load shedding has to be deaktivated 
;
Table Generator_data (g,*)
        Gen_cap     Gen_costs
g1      400         15
g2      400         20
g3      600         22
;
Table Demand_data (n,*)
        Need        LS_costs
n1      80          3000
n2      240         3000
n3      40          3000
n4      160         3000
n5      240         3000
n6      0           3000
;
$ontext
Table Line_data (n,bus,*)
          react      I_costs     L_cap        stat
n1.n2      0.40        40          100          1
n1.n4      0.60        60          80           1
n1.n5      0.20        20          100          1
n2.n3      0.20        20          100          1
n2.n4      0.40        40          100          1
n2.n6      0.30        30          100          0
n3.n5      0.20        20          100          1
n4.n6      0.30        30          100          0
;
$offtext
*$ontext
Table Line_data (n,bus,*)
          react      I_costs     L_cap        stat
n1.n2      0.40        40          100          1
n1.n3      0.38        38          100          0
n1.n4      0.60        60          80           1
n1.n5      0.20        20          100          1
n1.n6      0.68        68          70           0
n2.n3      0.20        20          100          1
n2.n4      0.40        40          100          1
n2.n5      0.31        31          100          0
n2.n6      0.30        30          100          0
n3.n4      0.59        59          82           0
n3.n5      0.20        20          100          1
n3.n6      0.48        48          100          0
n4.n5      0.63        63          75           0
n4.n6      0.30        30          100          0
n5.n6      0.61        61          78           0
;
*$offtext
Set
Map_lines(n,bus)
;
Map_lines(n,bus)$line_data(n,bus,'react') = yes
;
Map_lines(n,bus)$Map_lines(bus,n) = yes
;
Line_data(n,bus,'react')$Line_data(bus,n,'react') = Line_data(bus,n,'react')
;
Line_data(n,bus,'I_costs')$Line_data(bus,n,'I_costs') = Line_data(n,bus,'I_costs')
;
Line_data(n,bus,'L_cap')$Line_data(bus,n,'L_cap') = Line_data(n,bus,'L_cap')
;
Line_data(n,bus,'stat')$Line_data(bus,n,'stat') = Line_data(n,bus,'stat')
;
Line_data(n,bus,'b')$Map_lines(n,bus) = 1/Line_data(n,bus,'react')
;

Scalar ILmax    /900000/
*max invest budget
;
Scalar SIG      /8760/
*hours in a year
;
Scalar M
/1000/
;
Parameter
price(n)
;
Variables
Investment_costs
Costs
Power_flow(n,bus,k)
Theta(n)
;
positive Variables
Power_gen(g)
Load_shed(n)
;
binary variables
x(n,bus,k)
;
x.l(n,bus,k) = 1
;
x.fx(n,bus,k)$(Map_lines(n,bus) and ord (k) = 1 and Line_data(n,bus,'stat')) = 1
;


Equations
Total_costs
Line_investment
Balance


Ex_Line_neg_flow
Ex_line_pos_flow
Prosp_line_neg_flow
Prosp_line_pos_flow
Linearization_prosp_line_neg
Linearization_prosp_line_pos
Gen_det_up
LS_det

Theta_LB
Theta_UB
Theta_ref
;

Total_costs..                                costs =e=  sum((n,bus,k)$Map_lines(n,bus),  Line_data(n,bus,'I_costs') * x(n,bus,k)$(ord(k)>1 or Line_data(n,bus,'stat') = 0))
                                                  
                                                     +  sum(g,Generator_data(g,'Gen_costs') * power_gen(g))
                                                     +  sum(n,Demand_data(n,'LS_costs') * Load_shed(n))
                                                    
;
Line_investment..                             sum((n,bus,k)$Map_lines(n,bus),  Line_data(n,bus,'I_costs') * x(n,bus,k)$(ord(k)>1 or Line_data(n,bus,'stat') = 0))  =e= Investment_costs
;
Balance(n)..                                                Demand_data(n,'Need')
%NO_LS_Invest%                                                                     - Load_shed(n) 
                                                                                                    =e= sum(g$MapG(g,n),Power_gen(g)) 

                                                                                                     + sum((k,bus)$Map_lines(n,bus),Power_flow(bus,n,k))
                                                                                                     - sum((k,bus)$Map_lines(n,bus),Power_flow(n,bus,k))
;

Ex_line_neg_flow(n,bus,k)$Map_lines(n,bus)..                power_flow(n,bus,k) =g= - Line_data(n,bus,'L_cap')
;
Ex_line_pos_flow(n,bus,k)$Map_lines(n,bus)..                power_flow(n,bus,k) =l=   Line_data(n,bus,'L_cap')
;
Prosp_line_neg_flow(n,bus,k)$Map_lines(n,bus)..             power_flow(n,bus,k) =g= - x(n,bus,k) * Line_data(n,bus,'L_cap')    
;
Prosp_line_pos_flow(n,bus,k)$Map_lines(n,bus)..             power_flow(n,bus,k) =l=   x(n,bus,k) * Line_data(n,bus,'L_cap')   
;
Linearization_prosp_line_neg(n,bus,k)$Map_lines(bus,n)..    -(1-x(n,bus,k))*M   =l= power_flow(n,bus,k) - Line_data(n,bus,'b') * (Theta(n)-Theta(bus)) *100
;
Linearization_prosp_line_pos(n,bus,k)$Map_lines(n,bus)..    (1-x(n,bus,k))*M    =g= power_flow(n,bus,k) - Line_data(n,bus,'b') * (Theta(n)-Theta(bus)) *100
;
Gen_det_up(n,g)..                                             power_gen(g)  =l= Generator_data(g,'Gen_cap')
;
LS_det(n)..                                                 load_shed(n)  =l= Demand_data(n,'Need')
;
Theta_LB(n)..                                               -3.1415       =l= Theta(n)
;
Theta_UB(n)..                                               3.1415        =g= Theta(n)
;
Theta_ref(n)..                                              Theta(n)$ref(n) =l= 0
;

Model Stat_det_Tep /all/;

option optcr =0
;
solve Stat_det_Tep using MIP minimizing investment_costs;

price(n) = (balance.m(n))*(-1);

execute_unload "check.gdx";
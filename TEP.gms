Sets

n /n1*n5/
g /g1*g3/
d /d1*d5/
l /l1*l8/

exist (l)   /l1,l2/
prosp (l)   /l3*l8/

MapG (g,n)  /g1.n1,g2.n2,g3.n3/
MapD (d,n)  /d1.n1,d2.n4,d3.n5,d4.n2,d5.n3/
ref(n)      /n1/

Map_send_L  (l,n)  /l1.n1,l2.n2,l3.n3,l4.n5,l5.n3,l6.n4,l7.n1,l8.n2/
Map_res_L (l,n)    /l1.n4,l2.n5,l3.n2,l4.n3,l5.n4,l6.n5,l7.n5,l8.n1/
;
Table Generator_data (g,*)
        Gen_cap     Gen_costs
g1      350         15
g2      450         20
g3      300         22
;
Table Demand_data (d,*)
        Need        LS_costs
d1      200         70
d2      200         55
d3      250         45
d4      150         60
d5      200         65
;
Table Line_data (l,*)
        L_cap       I_costs     B
l1      150         0           500
l2      150         0           500
l3      100         500000      500
l4      150         700000      500
l5      100         300000      500
l6      50          100000      500
l7      125         350000      500
l8      200         1300000     500
;
Scalar ILmax    /900000/
*max invest budget
;
Scalar SIG      /8760/
*hours in a year
;
Scalar M
/5000/
;
Parameter
price(n)
;
Variables
Costs
Power_flow(l)
Theta(n)
;
positive Variables
Power_gen(g)
Load_shed (d)
;
Binary variables
x(l)
;
Equations
Total_costs
Line_investment
Balance
Ex_line_angle
Ex_Line_neg_flow
Ex_line_pos_flow
Prosp_line_neg_flow
Prosp_line_pos_flow
Linearization_prosp_line_neg
Linearization_prosp_line_pos
Gen_det
LS_det
Theta_LB
Theta_UB
Theta_ref
;

Total_costs..                costs =e= sum(l$prosp(l),Line_data(l,'I_costs')*x(l))
                                +SIG * (
                                        sum(g,Generator_data(g,'Gen_costs')*power_gen(g))
                                        +sum(d,Demand_data(d,'LS_costs')*Load_shed(d))
                                        )
;
Line_investment..            sum(l$prosp(l),Line_data(l,'I_costs')*x(l)) =l= ILmax
;
Balance(n)..                 sum(d$MapD(d,n),Demand_data(d,'Need')-Load_shed(d)) =e= sum(g$MapG(g,n),Power_gen(g))
                                                                                    -sum(l$Map_send_L(l,n),Power_flow(l))
                                                                                    +sum(l$Map_res_L(l,n),Power_flow(l))
;
Ex_line_angle(l)$exist(l)..                  power_flow(l) =e= -Line_data(l,'B')*(sum(n$Map_send_L(l,n),Theta(n))-sum(n$Map_res_L(l,n),Theta(n)))
;
Ex_line_neg_flow(l)$exist(l)..               power_flow(l) =g= -Line_data(l,'L_cap')
;
Ex_line_pos_flow(l)$exist(l)..               power_flow(l) =l=  Line_data(l,'L_cap')
;
Prosp_line_neg_flow(l)$prosp(l)..            power_flow(l) =g= -x(l) * Line_data(l,'L_cap')    
;
Prosp_line_pos_flow(l)$prosp(l)..            power_flow(l) =l=  x(l) * Line_data(l,'L_cap')
;
Linearization_prosp_line_neg(l)$prosp(l)..   -(1-x(l))*M   =l= power_flow(l) - Line_data(l,'B') * (sum(n$Map_send_L(l,n),Theta(n))-sum(n$Map_res_l(l,n),Theta(n)))
;
Linearization_prosp_line_pos(l)$prosp(l)..   (1-x(l))*M    =g= power_flow(l) - Line_data(l,'B') * (sum(n$Map_send_L(l,n),Theta(n))-sum(n$Map_res_l(l,n),Theta(n)))
;
Gen_det(g)..                                 power_gen(g)  =l= Generator_data(g,'Gen_cap')
;
LS_det(d)..                                  load_shed(d)  =l= Demand_data(d,'Need')
;
Theta_LB(n)..                                -3.1415       =l= Theta(n)
;
Theta_UB(n)..                                 3.1415       =g= Theta(n)
;
Theta_ref(n)..                             Theta(n)$ref(n) =l= 0
;
Model Stat_det_Tep /all/;
solve Stat_det_Tep using MIP minimizing costs;

price(n) = (balance.m(n)/sig)*(-1);

execute_unload "check.gdx";
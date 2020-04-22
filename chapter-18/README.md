# Chapter 18

## Exercise 18.1

Correlation plot below. As expected lots of very strong relationships between things that measure 'use' e.g. calls, charges, etc. 

![img](18.1/corr_plot.png)

Individual importances are:

|rowname                       |key |     value| rank|
|:-----------------------------|:---|---------:|----:|
|total_charge                  |yes | 0.6490411|    1|
|total_day_minutes             |yes | 0.6399666|    2|
|total_day_charge              |yes | 0.6399666|    3|
|total_minutes                 |yes | 0.6381385|    4|
|international_plan            |yes | 0.6091904|    5|
|number_customer_service_calls |yes | 0.6082071|    6|
|total_eve_minutes             |yes | 0.5726508|    7|
|total_eve_charge              |yes | 0.5726417|    8|
|voice_mail_plan               |yes | 0.5649036|    9|
|number_vmail_messages         |yes | 0.5616465|   10|
|total_intl_calls              |yes | 0.5606302|   11|
|total_intl_minutes            |yes | 0.5498979|   12|
|total_intl_charge             |yes | 0.5498979|   13|
|total_night_charge            |yes | 0.5281719|   14|
|total_night_minutes           |yes | 0.5281632|   15|
|total_day_calls               |yes | 0.5215742|   16|
|total_calls                   |yes | 0.5133228|   17|
|account_length                |yes | 0.5127787|   18|
|total_eve_calls               |yes | 0.5070339|   19|
|state                         |yes | 0.5052087|   20|
|total_night_calls             |yes | 0.5038491|   21|
|area_code                     |yes | 0.5024565|   22|

joint ones are

|name                          |     value| rank|
|:-----------------------------|---------:|----:|
|total_charge                  | 0.1966766|    1|
|total_minutes                 | 0.1869194|    2|
|total_day_charge              | 0.1637942|    3|
|total_day_minutes             | 0.1637800|    4|
|number_customer_service_calls | 0.1456000|    5|
|international_plan            | 0.1132000|    6|
|total_night_charge            | 0.1121260|    7|
|total_night_minutes           | 0.1120300|    8|
|total_eve_charge              | 0.1004576|    9|
|total_eve_minutes             | 0.1003939|   10|
|voice_mail_plan               | 0.0594000|   11|
|total_intl_charge             | 0.0520963|   12|
|total_intl_minutes            | 0.0519833|   13|
|total_night_calls             | 0.0491869|   14|
|total_eve_calls               | 0.0401255|   15|
|number_vmail_messages         | 0.0342314|   16|
|total_day_calls               | 0.0330364|   17|
|total_calls                   | 0.0287259|   18|
|account_length                | 0.0250287|   19|
|area_code                     | 0.0196000|   20|
|total_intl_calls              | 0.0187333|   21|
|state                         | 0.0048000|   22|

Can plot together. Very similar but as Relief is a joint measure some correlated things drop in importance.

![img](18.1/compare_plot.png)
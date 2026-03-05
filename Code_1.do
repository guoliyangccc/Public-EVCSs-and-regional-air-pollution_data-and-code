***Table 1: Summary statistics***
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 
sum pm25 PM25_NASA pm10 cumulative_station_count $XX1
sum IV2  //Slope_cable IV
sum IV3  //Policy_shock IV
sum pm25_primary pm25_secondary pm25_tertiary pm25_nonroads

///
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
gen pname2 = pname
replace pname2 = "冀北" if cityname == "唐山市" | cityname == "张家口市" | cityname == "秦皇岛市" | cityname == "承德市" | cityname == "廊坊市" 
replace pname2 = "蒙西" if pname == "内蒙古自治区"
replace pname2 = "蒙东" if cityname == "赤峰市" | cityname == "通辽市" | cityname == "兴安盟" | cityname == "呼伦贝尔市" 
replace pname2 = "深圳" if cityname == "深圳市"
drop if pname2 == "西藏自治区"
merge m:1 pname2 using electricity_price.dta
drop _merge

sum electricity_price

///
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
merge m:1 pname time_day using oil_price.dta
drop _merge

sum oil_price


///
use "/Users/guoliyangccc/Desktop/充电桩/Fast_EVCS.dta", clear
sum fast_lv

///
use "/Users/guoliyangccc/Desktop/充电桩/Smart_EVCS.dta", clear
sum Smart_I


///
gen region_type = 0
replace region_type = 1 if strpos(adname, "区") > 0

sum region_type



***Table2: Baseline results***
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 


//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month, absorb(month dow adcode) vce(cluster adcode)

//IV2
gen station = cumulative_station_count
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month , absorb(month dow adcode) vce(cluster adcode) first endog(station)

//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month , absorb(month dow adcode) vce(cluster adcode) first endog(station)



***Table3: Impact of public EVCSs on air pollution across areas with different traffic intensities***
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

merge 1:1 adcode time_day using pm25_primary.dta
keep if _merge==3
drop _merge

merge 1:1 adcode time_day using pm25_secondary.dta
keep if _merge==3
drop _merge

merge 1:1 adcode time_day using pm25_tertiary.dta
keep if _merge==3
drop _merge

merge 1:1 adcode time_day using pm25_nonroads.dta
keep if _merge==3
drop _merge

gen ln_pm25_primary = ln(pm25_primary)
gen ln_pm25_secondary = ln(pm25_secondary)
gen ln_pm25_tertiary = ln(pm25_tertiary)
gen ln_pm25_nonroads = ln(pm25_nonroads)

//OLS
reghdfe ln_pm25_primary cumulative_station_count $XX1 county_month , absorb(month dow adcode) vce(cluster adcode)
reghdfe ln_pm25_secondary cumulative_station_count $XX1 county_month , absorb(month dow adcode) vce(cluster adcode)
reghdfe ln_pm25_tertiary cumulative_station_count $XX1 county_month , absorb(month dow adcode) vce(cluster adcode)
reghdfe ln_pm25_nonroads cumulative_station_count $XX1 county_month , absorb(month dow adcode) vce(cluster adcode)

//IV2
ivreghdfe ln_pm25_primary (station = IV2) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)   
ivreghdfe ln_pm25_secondary (station = IV2) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station) 
ivreghdfe ln_pm25_tertiary (station = IV2) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station) 
ivreghdfe ln_pm25_nonroads (station = IV2) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station) 

//IV3
ivreghdfe ln_pm25_primary (station = IV3) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)   
ivreghdfe ln_pm25_secondary (station = IV3) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station) 
ivreghdfe ln_pm25_tertiary (station = IV3) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station) 
ivreghdfe ln_pm25_nonroads (station = IV3) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station) 



***Table4: Heterogeneity in air quality improvement by EVCS price characteristics***
//electricity price
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

use "/Users/guoliyangccc/Desktop/充电桩/electricity_price.dta", clear
sum electricity_price, detail //p50 = 420.7885

gen pname2 = pname
replace pname2 = "冀北" if cityname == "唐山市" | cityname == "张家口市" | cityname == "秦皇岛市" | cityname == "承德市" | cityname == "廊坊市" 
replace pname2 = "蒙西" if pname == "内蒙古自治区"
replace pname2 = "蒙东" if cityname == "赤峰市" | cityname == "通辽市" | cityname == "兴安盟" | cityname == "呼伦贝尔市" 
replace pname2 = "深圳" if cityname == "深圳市"
drop if pname2 == "西藏自治区"

merge m:1 pname2 using electricity_price.dta.dta
drop _merge

gen electricity_price_I = 0
replace electricity_price_I = 1 if electricity_price > 420.7885 //p50

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if electricity_price_I == 0, absorb(month dow adcode) vce(cluster adcode)
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if electricity_price_I == 1, absorb(month dow adcode) vce(cluster adcode)

//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if electricity_price_I == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if electricity_price_I == 1, absorb(month dow adcode) vce(cluster adcode) first endog(station)

//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if electricity_price_I == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if electricity_price_I == 1, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//mileage cost advantage
merge m:1 pname time_day using oil_price.dta
drop _merge

gen oil_electricity = oil_price / electricity_price
sum oil_electricity, detail // p50 = .2319597

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if oil_electricity <= .2319597, absorb(month dow adcode) vce(cluster adcode)
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if oil_electricity > .2319597, absorb(month dow adcode) vce(cluster adcode)

//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if oil_electricity <= .2319597, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if oil_electricity > .2319597, absorb(month dow adcode) vce(cluster adcode) first endog(station)

//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if oil_electricity <= .2319597, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if oil_electricity > .2319597, absorb(month dow adcode) vce(cluster adcode) first endog(station)



***Table5: Heterogeneity in air quality improvement by EVCS service characteristics***
//fast-charging EVCS
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

***查看样本期最后一天新增快充电站的描述性统计
use "/Users/guoliyangccc/Desktop/充电桩/Fast_EVCS.dta", clear
replace fast_lv = fast_lv * 100
sum fast_lv, detail

merge m:1 adcode using Fast_EVCS.dta
drop _merge

gen fast_I = 0
replace fast_I = 1 if fast_lv > 29.16667 //p50

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if fast_I == 0, absorb(month dow adcode) vce(cluster adcode)
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if fast_I == 1, absorb(month dow adcode) vce(cluster adcode)

//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if fast_I == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if fast_I == 1, absorb(month dow adcode) vce(cluster adcode) first endog(station)

//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if fast_I == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if fast_I == 1, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//Smart EVCS
use "/Users/guoliyangccc/Desktop/充电桩/Smart_EVCS.dta", clear
sum Smart_I

use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

merge m:1 adcode using Smart_EVCS.dta
drop _merge


//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if Smart_I == 0, absorb(month dow adcode) vce(cluster adcode)
reghdfe ln_mean_pm25 cumulative_station_count $XX1 county_month if Smart_I == 1, absorb(month dow adcode) vce(cluster adcode)

//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if Smart_I == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if Smart_I == 1, absorb(month dow adcode) vce(cluster adcode) first endog(station)

//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if Smart_I == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if Smart_I == 1, absorb(month dow adcode) vce(cluster adcode) first endog(station)



***Table6: Heterogeneity in air quality improvement by charging accessibility***
//Urban regions or Rural regions
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

gen region_type = 0
replace region_type = 1 if strpos(adname, "区") > 0

sum region_type

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if region_type == 1, absorb(month dow adcode) vce(cluster adcode)
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if region_type == 0, absorb(month dow adcode) vce(cluster adcode)

//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if region_type == 1, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if region_type == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)

//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if region_type == 1, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if region_type == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//vehicle-to-charger ratio
use "/Users/guoliyangccc/Desktop/充电桩/EV_stock_by_region_2023.dta", clear
gen car_point_lv = e_car / Pub_charging_points
sum car_point_lv, detail // p50 = 7.796922

use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

merge m:1 pname using EV_stock_by_region_2023.dta
drop _merge

gen car_point_lv_I = 0
replace car_point_lv_I = 1 if car_point_lv > 7.796922 //p50

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if car_point_lv_I == 0, absorb(month dow adcode) vce(cluster adcode)
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if car_point_lv_I == 1, absorb(month dow adcode) vce(cluster adcode)

//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if car_point_lv_I == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if car_point_lv_I == 1, absorb(month dow adcode) vce(cluster adcode) first endog(station)

//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if car_point_lv_I == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if car_point_lv_I == 1, absorb(month dow adcode) vce(cluster adcode) first endog(station)



***Figure 1 was generated using R 4.3.2***



***Figure 2 was generated using R 4.3.2***



***Figure 3 was generated using R 4.3.2***



***Figure 4 was generated using R 4.3.2***



***Figure 5 was generated using R 4.3.2***



***Figure 6***
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

//IV2
ivreghdfe ln_pm25 (c.station##c.station = c.IV2##c.IV2) $XX1 county_month , absorb(month dow adcode) vce(cluster adcode) first endog(station)

margins, dydx(station) at(station=(0(5)800))
marginsplot

//IV3
ivreghdfe ln_pm25 (c.station##c.station = c.IV3##c.IV3) $XX1 county_month , absorb(month dow adcode) vce(cluster adcode) first endog(station)

margins, dydx(station) at(station=(0(5)800))
marginsplot

//The regression results were plotted as Figure 6 using R version 4.3.2.


***Figure 7***
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

//1.PM25 from NASA
gen ln_PM25_NASA = ln(PM25_NASA)
//OLS
reghdfe ln_PM25_NASA cumulative_station_count $XX1 county_month, absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_PM25_NASA (station = IV2) $XX1 county_month , absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_PM25_NASA (station = IV3) $XX1 county_month , absorb(month dow adcode) vce(cluster adcode) first endog(station)


//2.PM10 from NASA
gen ln_pm10 = ln(pm10)
//OLS
reghdfe ln_pm10 cumulative_station_count $XX1 county_month , absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_pm10 (station = IV2) $XX1 county_month , absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm10 (station = IV3) $XX1 county_month , absorb(month dow adcode) vce(cluster adcode) first endog(station)


//3.Policy 1
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

drop if cityname == "北京市" | cityname == "天津市" | cityname == "上海市"
drop if pname == "江苏省"

drop if cityname == "石家庄市" | cityname == "唐山市" | cityname == "秦皇岛市" | cityname == "邯郸市" | cityname == "邢台市" | cityname == "保定市" | cityname == "沧州市" | cityname == "廊坊市" | cityname == "衡水市" | cityname == "辛集市" | cityname == "定州市" //河北

drop if cityname == "济南市" | cityname == "淄博市" | cityname == "枣庄市" | cityname == "东营市" | cityname == "潍坊市" | cityname == "济宁市" | cityname == "泰安市" | cityname == "日照市" | cityname == "临沂市" | cityname == "德州市" | cityname == "聊城市" | cityname == "滨州市" | cityname == "菏泽市" //山东


drop if cityname == "郑州市" | cityname == "开封市" | cityname == "洛阳市" | cityname == "平顶山市" | cityname == "安阳市" | cityname == "鹤壁市" | cityname == "新乡市" | cityname == "焦作市" | cityname == "濮阳市" | cityname == "许昌市" | cityname == "漯河市" | cityname == "三门峡市" | cityname == "商丘市" | cityname == "周口市" | cityname == "济源市" //河南

drop if cityname == "杭州市" | cityname == "宁波市" | cityname == "嘉兴市" | cityname == "湖州市" | cityname == "绍兴市" | cityname == "舟山市" //浙江

drop if cityname == "合肥市" | cityname == "芜湖市" | cityname == "蚌埠市" | cityname == "淮南市" | cityname == "马鞍山市" | cityname == "淮北市" | cityname == "滁州市" | cityname == "阜阳市" | cityname == "宿州市" | cityname == "六安市" | cityname == "亳州市" //安徽

drop if cityname == "太原市" | cityname == "阳泉市" | cityname == "长治市" | cityname == "晋城市" | cityname == "晋中市" | cityname == "运城市" | cityname == "临汾市" | cityname == "吕梁市" //山西

drop if cityname == "西安市" | cityname == "铜川市" | cityname == "宝鸡市" | cityname == "咸阳市" | cityname == "渭南市" //陕西

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month , absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month , absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month , absorb(month dow adcode) vce(cluster adcode) first endog(station)


//4.Policy 2
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

gen fuel_vehicle = 0

replace fuel_vehicle = 1 if cityname == "北京市" //限五环以内
replace fuel_vehicle = 1 if adname == "右江区" //百色市右江区解放街
replace fuel_vehicle = 1 if adname == "南关区" //‌长春市净月潭风景名胜区
replace fuel_vehicle = 1 if time_day <= date("08feb2024", "DMY") & (adname == "岳麓区")  //长沙市橘子洲,2024年02月08日-2024年02月16日除外
replace fuel_vehicle = 1 if time_day >= date("16feb2024", "DMY") & (adname == "岳麓区")  //长沙市橘子洲,2024年02月08日-2024年02月16日除外
replace fuel_vehicle = 1 if cityname == "成都市" //限四环以内
replace fuel_vehicle = 1 if cityname == "重庆市"
replace fuel_vehicle = 1 if adname == "运河区" //沧州市运河区西堤顶路
replace fuel_vehicle = 1 if adname == "湘桥区" //潮州市湘桥区涸溪桥
replace fuel_vehicle = 1 if adname == "甘井子区" //大连北站
replace fuel_vehicle = 1 if adname == "东莞市"
replace fuel_vehicle = 1 if adname == "顺德区" //佛山市顺德区
replace fuel_vehicle = 1 if time_day >= date("11mar2024", "DMY") & (adname == "上思县") //防城港上思县月亮桥,2024年03月11日起
replace fuel_vehicle = 1 if cityname == "广州市"
replace fuel_vehicle = 1 if cityname == "杭州市"
replace fuel_vehicle = 1 if time_day >= date("10sep2024", "DMY") & (cityname == "合肥市") //2024年09月10日起
replace fuel_vehicle = 1 if adname == "金湖县" //安市金湖县
replace fuel_vehicle = 1 if adname == "武安市" //邯郸市武安市新兴大街北段
replace fuel_vehicle = 1 if adname == "惠阳区" //惠州市惠阳区
replace fuel_vehicle = 1 if adname == "平遥县" //晋中市平遥县
replace fuel_vehicle = 1 if adname == "平湖市" //嘉兴市平湖市
replace fuel_vehicle = 1 if adname == "蓬江区" //江门市蓬江区
replace fuel_vehicle = 1 if time_day >= date("24apr2024", "DMY") & (adname == "栾川县") //洛阳市栾川县，2024年04月24日起
replace fuel_vehicle = 1 if cityname == "兰州市"
replace fuel_vehicle = 1 if adname == "东海县" //连云港东海县
replace fuel_vehicle = 1 if cityname == "吕梁市" //连云港东海县
replace fuel_vehicle = 1 if adname == "莲都区" //丽水市莲都区白云国家森林公园
replace fuel_vehicle = 1 if adname == "会理市" //凉山彝族自治州会理市
replace fuel_vehicle = 1 if adname == "游仙区" //绵阳市游仙区
replace fuel_vehicle = 1 if time_day >= date("25may2024", "DMY") & (adname == "东坡区") //眉山市东坡区，2024年05月25日起
replace fuel_vehicle = 1 if time_day >= date("01apr2024", "DMY") & (adname == "市南区") //青岛市南区，2024年04月01日至2024年11月30日
replace fuel_vehicle = 1 if time_day <= date("30nov2024", "DMY") & (adname == "市南区") //青岛市南区，2024年04月01日至2024年11月30日
replace fuel_vehicle = 1 if cityname == "上海市"
replace fuel_vehicle = 1 if adname == "相城区" //苏州相城区
replace fuel_vehicle = 1 if cityname == "深圳市"
replace fuel_vehicle = 1 if adname == "天涯区" //三亚市天涯区
replace fuel_vehicle = 1 if adname == "万柏林区" //太原市万柏林区
replace fuel_vehicle = 1 if cityname == "天津市" //太原市万柏林区
replace fuel_vehicle = 1 if time_day >= date("02jan2024", "DMY") & (cityname == "渭南市") //渭南市，2024年01月02起
replace fuel_vehicle = 1 if time_day >= date("25feb2024", "DMY") & (adname == "汉阳区") //武汉市汉阳区,2024年02月25日至2029年03月04日
replace fuel_vehicle = 1 if adname == "新市区" //乌鲁木齐市新市区
replace fuel_vehicle = 1 if adname == "天山区" //乌鲁木齐市天山区
replace fuel_vehicle = 1 if adname == "长洲区" //梧州市长洲区
replace fuel_vehicle = 1 if adname == "龙圩区‌‌" //梧州市龙圩区‌‌
replace fuel_vehicle = 1 if adname == "集美区" //厦门市‌‌集美区
replace fuel_vehicle = 1 if time_day >= date("10feb2024", "DMY") & (adname == "高陵区") //西安市高陵区，2024年02月10日至2024年11月08日
replace fuel_vehicle = 1 if time_day <= date("08nov2024", "DMY") & (adname == "高陵区") //西安市高陵区，2024年02月10日至2024年11月08日
replace fuel_vehicle = 1 if time_day >= date("10feb2024", "DMY") & (adname == "新城区") //西安市新城区，2024年02月10日至2024年11月08日
replace fuel_vehicle = 1 if time_day <= date("08nov2024", "DMY") & (adname == "新城区") //西安市新城区，2024年02月10日至2024年11月08日
replace fuel_vehicle = 1 if time_day >= date("10feb2024", "DMY") & (adname == "长安区") //西安市长安区，2024年02月10日至2024年11月08日
replace fuel_vehicle = 1 if time_day <= date("08nov2024", "DMY") & (adname == "长安区") //西安市长安区，2024年02月10日至2024年11月08日
replace fuel_vehicle = 1 if adname == "渭城区" & time_day >= date("25feb2024", "DMY") & time_day <= date("08nov2024", "DMY") //2024年01月02日至2024年02月09日、2024年02月25日至2024年11月08日
replace fuel_vehicle = 1 if adname == "渭城区" & time_day >= date("02jan2024", "DMY") & time_day <= date("09feb2024", "DMY")
replace fuel_vehicle = 1 if time_day >= date("01aug2024", "DMY") & (adname == "泉山区") //徐州市泉山区,2024年08月01日起
replace fuel_vehicle = 1 if adname == "蓬莱区" //烟台市蓬莱区
replace fuel_vehicle = 1 if adname == "伊宁市" //伊犁哈萨克自治州伊宁市
replace fuel_vehicle = 1 if time_day >= date("16may2024", "DMY") & (adname == "横山区") //榆林市横山区，2024年05月16日起

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month fuel_vehicle, absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month fuel_vehicle, absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month fuel_vehicle, absorb(month dow adcode) vce(cluster adcode) first endog(station)



//5.Policy 3 （Regions that implemented the policy during the entire sample period are absorbed by the fixed effects）
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

gen parking_discount = 0
replace parking_discount = 1 if time_day >= date("01jan2024", "DMY") & (pname == "山东省") //山东自2024年1月1日开始实施
replace parking_discount= 1 if time_day >= date("23nov2024", "DMY") & (cityname == "西安市") //西安自2024年11月23日起，每日免费一次2小时
replace parking_discount = 1 if time_day >= date("01mar2024", "DMY") & (adname == "麻江县") //贵州麻江县自2024年3月1日起
replace parking_discount = 1 if time_day >= date("01mar2024", "DMY") & (cityname == "苏州市") //苏州自2024年3月1日起
replace parking_discount = 1 if time_day <= date("31dec2023", "DMY") & (cityname == "温州市") //温州市新能源汽车停车费的优惠政策有效期至2023年12月31日截止
replace parking_discount = 1 if time_day <= date("31dec2023", "DMY") & (cityname == "嘉兴市") //嘉兴市新能源汽车停车费的优惠政策有效期为2023年1月1日至2023年12月31日

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month parking_discount, absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month parking_discount, absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month parking_discount, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//6.Policy 1-3
//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month fuel_vehicle parking_discount, absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month fuel_vehicle parking_discount, absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month fuel_vehicle parking_discount, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//7.time_day FE
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month, absorb(adcode time_day) vce(cluster adcode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month, absorb(adcode time_day) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month, absorb(adcode time_day) vce(cluster adcode) first endog(station)

//8.year by month FE
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 
egen year_month = group(year month)

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month, absorb(adcode year_month dow) vce(cluster adcode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month, absorb(adcode year_month dow) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month, absorb(adcode year_month dow) vce(cluster adcode) first endog(station)


//9.county_month_FE
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 
egen county_month_FE = group(adcode month)

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 , absorb(county_month_FE dow) vce(cluster adcode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 , absorb(county_month_FE dow) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 , absorb(county_month_FE dow) vce(cluster adcode) first endog(station)


//10.County-specific week trend
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 c.week#i.adcode, absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 c.week#i.adcode, absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 c.week#i.adcode, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//11.City level clustering
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month, absorb(month dow adcode) vce(cluster citycode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month, absorb(month dow adcode) vce(cluster citycode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month, absorb(month dow adcode) vce(cluster citycode) first endog(station)

*12.Province level clustering
//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month, absorb(month dow adcode) vce(cluster pcode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month, absorb(month dow adcode) vce(cluster pcode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month, absorb(month dow adcode) vce(cluster pcode) first endog(station)


//13.Without coal-fired power plants
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 
merge m:1 adcode using 区县是否含火电厂.dta
keep if _merge == 3
drop _merge

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if has_coal_power == 0, absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if has_coal_power == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if has_coal_power == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//14.Without province level cities
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

drop if cityname == "北京市" | cityname == "天津市" | cityname == "上海市"
drop if cityname == "重庆市" 

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month, absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//15.Without semi-province level cities
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

drop if cityname == "广州市" | cityname == "深圳市" | cityname == "成都市" | cityname == "杭州市" | cityname == "南京市" | cityname == "武汉市" | cityname == "大连市" | cityname == "青岛市" | cityname == "济南市" | cityname == "宁波市"

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month, absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//16.Without cities in (14) & (15)
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

drop if cityname == "北京市" | cityname == "天津市" | cityname == "上海市"
drop if cityname == "重庆市" 

drop if cityname == "广州市" | cityname == "深圳市" | cityname == "成都市" | cityname == "杭州市" | cityname == "南京市" | cityname == "武汉市" | cityname == "大连市" | cityname == "青岛市" | cityname == "济南市" | cityname == "宁波市"

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month, absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//17.Drop outliers = 1%
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

sum pm25, detail
drop if pm25 <= .140988
drop if pm25 >= 213.2992

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month, absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//18.Daily max. of PM2.5
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

merge 1:1 adcode time_day using daily_pm25_max.dta
drop if _merge != 3
drop _merge

gen ln_max_pm25 = ln(pm25_max)

//OLS
reghdfe ln_max_pm25 cumulative_station_count $XX1 county_month , absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_max_pm25 (station = IV2) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_max_pm25 (station = IV3) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//19.Daily s.d. of PM2.5
merge 1:1 adcode time_day using daily_pm25_sd.dta
drop if _merge != 3
drop _merge

gen ln_sd_pm25 = ln(pm25_sd)

//OLS
reghdfe ln_sd_pm25 cumulative_station_count $XX1 county_month, absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_sd_pm25 (station = IV2) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_sd_pm25 (station = IV3) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//20.Daily range of PM2.5
merge 1:1 adcode time_day using daily_pm25_range.dta
drop if _merge != 3
drop _merge

gen ln_pm25_range = ln(pm25_range)

//OLS
reghdfe ln_pm25_range cumulative_station_count $XX1 county_month, absorb(month dow adcode) vce(cluster adcode)
//IV2
ivreghdfe ln_pm25_range (station = IV2) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)
//IV3
ivreghdfe ln_pm25_range (station = IV3) $XX1 county_month, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//The regression results were plotted as Figure 7 using R version 4.3.2.



***Figure 8 was generated using ArcGIS 10.2***



***Figure 9 was generated using R 4.3.2***



***Figure 10***
//Exposure distance to coal-fired power plants
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

merge m:1 adcode using 区县0-50km是否含火电厂.dta
keep if _merge == 3
drop _merge

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if plant_nearby_10 == 1, absorb(month dow adcode) vce(cluster adcode) // <=10km
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if plant_nearby_10 == 0, absorb(month dow adcode) vce(cluster adcode) // >10km
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if plant_nearby_20 == 0, absorb(month dow adcode) vce(cluster adcode) // >20km
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if plant_nearby_30 == 0, absorb(month dow adcode) vce(cluster adcode) // >30km
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if plant_nearby_40 == 0, absorb(month dow adcode) vce(cluster adcode) // >40km
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if plant_nearby_50 == 0, absorb(month dow adcode) vce(cluster adcode) // >50km

//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if plant_nearby_10 == 1, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if plant_nearby_10 == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if plant_nearby_20 == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if plant_nearby_30 == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if plant_nearby_40 == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if plant_nearby_50 == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)

//IV3
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if plant_nearby_10 == 1, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if plant_nearby_10 == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if plant_nearby_20 == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if plant_nearby_30 == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if plant_nearby_40 == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if plant_nearby_50 == 0, absorb(month dow adcode) vce(cluster adcode) first endog(station)


//Quantiles of the non-fossil energy generation share
use "/Users/guoliyangccc/Desktop/充电桩/2024年发电量情况.dta", clear
sum Non_fossil_elec_share, detail  // Non-fossil electricity generation share: q25 = 18.29; q50 = 25.09; q75 = 42.76 

use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

merge m:1 pname using 2024年发电量情况.dta
drop _merge

//OLS
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if Non_fossil_elec_share < 18.29, absorb(month dow adcode) vce(cluster adcode) //q0-q25
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if Non_fossil_elec_share >= 18.29 & Non_fossil_elec_share < 25.09, absorb(month dow adcode) vce(cluster adcode) //q25-q50
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if Non_fossil_elec_share >= 25.09 & Non_fossil_elec_share < 42.76, absorb(month dow adcode) vce(cluster adcode) //q50-q75
reghdfe ln_pm25 cumulative_station_count $XX1 county_month if Non_fossil_elec_share >= 42.76, absorb(month dow adcode) vce(cluster adcode) //q75-q100

//IV2
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if Non_fossil_elec_share < 18.29, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if Non_fossil_elec_share >= 18.29 & Non_fossil_elec_share < 25.09, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if Non_fossil_elec_share >= 25.09 & Non_fossil_elec_share < 42.76, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV2) $XX1 county_month if Non_fossil_elec_share >= 42.76, absorb(month dow adcode) vce(cluster adcode) first endog(station)

**IV2估计
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if Non_fossil_elec_share< 18.29, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if Non_fossil_elec_share >= 18.29 & Non_fossil_elec_share < 25.09, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if Non_fossil_elec_share >= 25.09 & Non_fossil_elec_share < 42.76, absorb(month dow adcode) vce(cluster adcode) first endog(station)
ivreghdfe ln_pm25 (station = IV3) $XX1 county_month if Non_fossil_elec_share >= 42.76, absorb(month dow adcode) vce(cluster adcode) first endog(station)




***Supplementary Figure A1***
use "/Users/guoliyangccc/Desktop/充电桩/Panel_A2.dta", clear
global XX1 "wind_speed cp t2m 平均夜间灯光亮度_interp" 

bysort adcode (time_day): keep if time_day == td(08dec2024)

gen group6 = ceil(cumulative_station_count/10)
replace group6 = 6 if group6 > 5
label define g6 1 "1–10" 2 "11–20" 3 "21–30" 4 "31–40" 5 "41–50" 6 "50+"
label values group6 g6
tab group6

graph bar (count), over(group6, label()) ///
	note("Interval Ranges", size(medium) position(6) span) ///
    ytitle("Number of EVCS") ///
    blabel(bar)
	
	
***Supplementary Figure A2 was generated using ArcGIS 10.2***

***Supplementary Figure A3 was generated using R 4.3.2***
	

***Supplementary Table B1-B20, see the code above for Figure 7***
	

***Supplementary Table B21, see the code above for Table 4-6***


***Supplementary Table B22-23, see the code above for Figure 10***

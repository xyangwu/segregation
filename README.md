## Occupational Gender Segregation， Labor Force Participation

## Census data in China

#### 隔离指数的测量：D指数、Ds指数、A指数

- `datatidy_labforce.R`, labor force data cleaning (处理劳动力人口数据)
- `laborforce_country.R`, labor force population at country level (国家层面的劳动力)
- `laborforce_city.R`, labor force population at city level (城市层面的劳动力)

- `datatidy_occup.R` , aggregate the number of workers in each occupation (处理不同职业从业人口)

- `occupation_country.R`, occupation at country level (国家层面的劳动力) 

- `fine.R`, compute 'one child policy' fines (计划生育罚金)

- `reference.R `, references of other papers and researches in this project

  #### Model

- `model_labforce.R ` , model of labor force data (对地市级市劳动力人口建立模型)

- `model_occup.R` ,  model of occupation segregation (对地级市的职业隔离指数建模)

<p align="center">
<img src="https://raw.githubusercontent.com/xyangwu/segregation/master/export/female_map_1.png" alt="female_map_1" width="500" title="就业人口中的女性比例">
</p>

<p align="center">图1 👆</p>
<p align="center">
<img src="https://raw.githubusercontent.com/xyangwu/segregation/master/export/dissim_map_1.png" alt="dissim_map_1" width="500" title="隔离指数">
</p>

<p align="center">图2 👆</p>
#### Visualization

- ` vis_labour.R` , charts and table for present labor force participation
- `vis_occup.R`, charts and table for present pccupation segregation 



### 职业分类

职业分类标准1990、2000、2010



### **行政区划变动**

​		三轮人口普查的地理单位不是完全对应的，部分行政区划单位经历了调整（表1）。1990年、2000年和2010年人口普查的地理单位按照曾隶属或新划归的行政单位进行合并，例如：哈尔滨市阿城区在1990年普查时是单列的阿城县（哈尔滨代管），本研究把阿城县归到哈尔滨市进行分析；2000年设立的吉林省松原市曾隶属于白城专区（现在的白城市），本研究把松原市被归到白城市。因而实际的分析单位数量都少于各年份人口普查的地理单位。合并为更大的地理单位可以消除行政区划变动造成的分析单位不一致，当然另一种更为精确的方法是把区县/街道一级的数据按调整后的地级区划单位重新汇总，可避免因为合并造成的分析单位数量减少。重新汇总的方法有一个缺点是忽视了行政界限所暗示的社会意义，行政权力在我国是非常普遍和重要的资源分配方式。涉及空间单位的量化研究常常为保证数据模式和样本数量而踟蹰难进，权衡两者后本研究选择合并地理单位来获得分析单位的统一性（附录）。

**表1** 2000年以后新设的地级市在1990年隶属的地级行政单位

| 省     | 2010区划       | 2000区划       | 1990区划                     | 统一后单位       |
| ------ | -------------- | -------------- | ---------------------------- | ---------------- |
| 黑龙江 | 哈尔滨市阿城区 | 哈尔滨市阿城区 | 阿城市                       | 哈尔滨，市阿市   |
| 吉林   | 松原市         | 松原市         | 白城市(白城专区)             | 白城市，松原市   |
| 江苏   | 泰州市         | 泰州市         | 扬州市                       | 扬州市，泰州市   |
| 江苏   | 宿迁市         | 宿迁市         | 淮阴市                       | 淮阴市，宿迁市   |
| 安徽   | 亳州市         | 亳州市         | 阜阳市(阜阳地区)             |                  |
| 山东   | 莱芜市         | 莱芜市         | 泰安市                       |                  |
| 湖北   | 随州市         | 随州市         | 湖北省直辖市                 |                  |
| 广东   | 揭阳市         | 揭阳市         | 汕头市(汕头地区)             |                  |
| 广东   | 云浮市         | 云浮市         | 肇庆市                       |                  |
| 广西   | 防城港市       | 防城港市       | 钦州地区(北海，钦州，防城港) | 钦州市，防城港市 |
| 广西   | 贵港市         | 贵港市         | 玉林市(玉林地区)             |                  |
| 四川   | 眉山市         | 眉山市         | 乐山市（眉山地区）           |                  |
| 四川   | 广安市         | 广安市         | 南充市(南充专区)             |                  |
| 四川   | 巴中市         | 巴中市         | 达州市(达县地区)             |                  |
| 四川   | 资阳市         | 资阳市         | 内江市                       |                  |
|        |                |                |                              |                  |



> 1996年8月12日，经国务院批准，调整扬州市行政区划，“扬泰分设”：县级**泰州市**从**扬州市**划出，组建地级泰州市，下辖海陵区、靖江市、泰兴市、姜堰市、兴化市。[links](https://baike.baidu.com/item/%E6%B3%B0%E5%B7%9E/26886?fromtitle=%E6%B3%B0%E5%B7%9E%E5%B8%82&fromid=11044102)

> 1993年5月23日，国务院批准撤销**防城各族自治县**和防城港区，设立**防城港市**（地级），以原防城各族自治县和防城港区的行政区域为防城港市的行政区域，将钦州地区的**上思县**划归防城港市领导。防城港市辖**防城、港口**两个区和上**思县**。同时，广西壮族自治区人民政府决定，原**东兴经济开发区**（现为县级市）划归防城港市领导，按处级建制管理。[links](https://baike.baidu.com/item/%E9%98%B2%E5%9F%8E%E6%B8%AF/992284?fromtitle=%E9%98%B2%E5%9F%8E%E6%B8%AF%E5%B8%82&fromid=9411063#1)

> 广安市: 广安区, 前锋区, 岳池县, 武胜县, 邻水县, 华蓥市



## Missing Data

部分省份未公开普查汇总数据

| province | year |
| -------- | ---- |
| 福建     | 2000 |
| 天津     | 2000 |
| 云南     | 2000 |
| 河南     | 2000 |
| 重庆     | 2000 |
| 新疆     | 2000 |
| 黑龙江   | 2000 |
| 吉林     | 2000 |



#### Still updating...😆



# segregation

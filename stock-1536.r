#這個主要是使用R來用於台灣股市的線圖繪製與回測策略
#參考__http://www.bituzi.com/2014/12/Rbacktest6mins.html#uds-search-results 安裝
#台灣股市的語法

#安裝quantmod套件
install.packages('quantmod')
library(quantmod)

h=getSymbols("3003.TW",auto.assign=FALSE) #下載股市資料 getSymbols並定義一個字母h 以1536為例子
h	#抓取1536資料
chartSeries(h)	#繪製股價歷史線圖,所有資料點
chartSeries(h["2012-01::2016-03"],theme="white")	#如果我只想要看2012年1月到2016年3月的圖形,背景換白色

#第四步：畫出20日平均線(月線)，60日平均線(季線)。[,4]表示收盤價的值
ma_20<-runMean(h[,4],n=20)  
ma_60<-runMean(h[,4],n=60)
addTA(ma_20,on=1,col="blue")
addTA(ma_60,on=1,col="red")

addBBands()	#套件quantmod也包含了其他的技術指標，最常用的我想就是布靈通道。
addBBands(draw="p")	#也可另外加上Bollinger%b的圖形。Bollinger%b = (Close-LowerBound) / (UpperBound-LowerBound)
addMACD()
#第五步:進行策略回測ex. 普通的均線策略：當20ma大於60ma時，全壓買進；當20ma小於60ma時，做空。輸入指令
position<-Lag(ifelse(ma_20>ma_60, 1,0))	#解說：position為一個時間序列，以日為單位，如果20ma大於60ma，設值為1；否則設值為0空手,-1做空。由於我們是日資料，訊號發生時只能隔天做交易，故將這向量全部往後遞延一天。
return<-ROC(Cl(h))*position	#ROC計算：log(今天收盤價/昨天收盤價)，乘上poistion代表。若1則持有，若0則空手
return<-return['2012-01-30/2016-03-21']	#由於我們策略條件是60ma>20ma之後才會交易，故統計值從2007-03-20開始；另外在有配發股利情況下容易失真
return<-exp(cumsum(return))	#cumsum計算累計值，即將每一分量之前的值累加起來。取exp函數是要計算累計損亦。(這裡運用國中數學:log(a)+log(b)=log(ab)，exp(log(ab))=ab)
plot(return)	#解說：將累計損益,此策略的損益圖形如下，橫軸為時間軸，縱軸為報酬率，1代表原始自有資金100%。

#http://investget.blogspot.tw/2010/12/2.html  回測目標：均線通道策略可以賺錢嗎？ 回測時間：從2002/1/2到2012/10/25。
#進場方式：(1)當收盤價大於上通道時，價格突破最近6根K棒的最高點時進場做多。
#		  (2)當收盤價小於下通道時，價格突破最近6根K棒的最低點時進場做空。

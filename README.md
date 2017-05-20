# rsloan-dcna

R Function for Row NA .

## Detail

為 R  Data.Frame 橫列處理 NA 的 Function 。

```
有實際成績的科目欄位 / 所有科目欄位
```

![nca](https://cloud.githubusercontent.com/assets/6993715/26279715/893f7918-3ded-11e7-91c4-db87b005d4ef.PNG)

```
# weg -> 0.70

nac = function( data, weg){
     ## data.frame row 轉換成向量
     dfvtr = function( data, dfrow){
          vtcda = as.numeric(data[ dfrow, 2:NCOL(data)])
          vtcda
     }

     ## 求算 NA 比率
     nacllog = function( data, dcol){
          # 權重 DATA
          na2dt = dfvtr( data, dcol)

          # 權重分子 molecular
          n2dmcr = length(na.omit(na2dt))

          # 權重分母 Denominator
          n2dtmr = length(na2dt)

          nall = n2dmcr/n2dtmr
          nall
     }

    ## NA 基準合併成向量
    nabc = function(data){
        d = numeric()
        for( ncg in 1:NROW(data)){
            k = nacllog( data, ncg)
            d = c( d, k)
        }
        d
    }

    ## 篩選
    data[!(nabc(data) < weg),]
}
```

- `data` 參數為 Data.Frame 物件。

- `weg` 參數為 NA 值比例。




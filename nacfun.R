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
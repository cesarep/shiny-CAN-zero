XE = xmin; XD = xmax
FXE = f(xmin); FXD = f(xmax)
XM = (xmin+xmax)/2; FXM = f((xmin+xmax)/2)
i = 1
while(abs(FXM[i]) > prec){
	if(FXE[i]*FXM[i] < 0){
		XE[i+1] = XE[i]
		XD[i+1] = XM[i]
	}
	else if(FXD[i]*FXM[i] < 0){
		XE[i+1] = XM[i]
		XD[i+1] = XD[i]
	}
	else{
		status = list(s = FALSE, msg = "Não há raiz no intervalo")
		break
	}
	FXE[i+1] = f(XE[i+1])
	FXD[i+1] = f(XD[i+1])
	XM[i+1] = (XE[i+1]+XD[i+1])/2
	FXM[i+1] = f(XM[i+1])
	i = i+1
	if(i > 50){
		status = list(s = FALSE, msg = "50 iterações excedido")
		break
	}
	status = list(s = TRUE, xf = XM[i], fxf = FXM[i], cab="<tr>
	<th>\\(i\\)</th>
	<th>\\(x_{esq}\\)</th>
	<th>\\(f(x_{esq})\\)</th>
	<th>\\(x_{dir}\\)</th>
	<th>\\(f(x_{dir})\\)</th>
	<th>\\(x_{medio}\\)</th>
	<th>\\( f(x_{medio})\\)</th>
</tr>")
}

tabela <- cbind(i = 1:i, XE, FXE, XD, FXD, XM, FXM)
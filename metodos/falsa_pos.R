XE = xmin; XD = xmax
FXE = f(xmin); FXD = f(xmax)
XR = XE-FXE*(XD-XE)/(FXD-FXE)
FXR = f(XR)

i = 1
while(abs(FXR[i]) > prec){
   if(FXE[i]*FXR[i] < 0){
      XE[i+1] = XE[i]
      XD[i+1] = XR[i]
   }
   else if(FXD[i]*FXR[i] < 0){
      XE[i+1] = XR[i]
      XD[i+1] = XD[i]
   }
   else{
      status = list(s = FALSE, msg = "Não há raiz no intervalo")
      break
   }
   FXE[i+1] = f(XE[i+1])
   FXD[i+1] = f(XD[i+1])
   XR[i+1] = XE[i+1]-FXE[i+1]*(XD[i+1]-XE[i+1])/(FXD[i+1]-FXE[i+1])
   FXR[i+1] = f(XR[i+1])
   i = i+1
   if(i > 50){
      status = list(s = FALSE, msg = "50 iterações excedido")
      break
   }
   status = list(s = TRUE, xf = XR[i], fxf = FXR[i], cab="<tr>
	<th>\\(i\\)</th>
	<th>\\(x_{esq}\\)</th>
	<th>\\(f(x_{esq})\\)</th>
	<th>\\(x_{dir}\\)</th>
	<th>\\(f(x_{dir})\\)</th>
	<th>\\(x_{raiz}\\)</th>
	<th>\\( f(x_{raiz})\\)</th>
</tr>")
}

tabela <- cbind(i = 1:i, XE, FXE, XD, FXD, XR, FXR)
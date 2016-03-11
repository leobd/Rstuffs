colorvec<-function(topo1,topo2,palet="blue2red",intervales=100)
{
	require(colorRamps)
	distp2p<-function(a,b){
		vecdist<-NULL
		for (i in 1:dim(a)[1]){
		vecdist<-c(vecdist,sqrt((b[i,1]-a[i,1])^2+(b[i,2]-a[i,2])^2+(b[i,3]-a[i,3])^2))
		}
		print(vecdist)
		}
		
	dit<-distp2p(topo1,topo2)
	dit2<-cut(as.numeric(dit),breaks = intervales)
		if (palet=="blue2red") idcolor<-blue2red(intervales)
		if (palet=="blue2green") idcolor<-blue2green(intervales)
		if (palet=="green2red") idcolor<-green2red(intervales)
	for (i in 1:length(dit))
	{
		tmpseg<-rbind(topo1[i,],topo2[i,])
		segments3d(tmpseg,color=idcolor[as.numeric(dit2[i])])
	}
}

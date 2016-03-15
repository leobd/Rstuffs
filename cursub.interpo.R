############################################################################
##                    RESAMPLING WITH INTERPOLATIO		                    ##
############################################################################

cursub.interpo<-function(cur,req)
{
mat<-as.matrix(dist(cur))
DPO<-NULL																											#DPO= original point to point distance
	for (j in 1:nrow(cur)-1)
		{
			a<-mat[j+1,j]
			DPO<-c(DPO,a)
		}
DCO<-NULL																											#DCO= original cordal distance to initial point
	for (j in 1:length(DPO))
		{
			DCO[j]<-sum(DPO[1:j])
		}

DN<-sum(DPO)/(req+1)																					#total cordal distance divided by the number of points = distance between points we will create (+1 as for n required points, there is n+1 intervals)
DCN<-DN*(1:(req))  																						#DCN= the new cordal distance to the initial point
proxima <- matrix(nrow=req,ncol=2)														#matrix containing the 2 closest points
for (k in 1:length(DCN))
	{
		first <- which.min(abs(DCO- DCN[k]))											#closest point
		proxima[k,1] <- first
		second <- which.min(abs(DCO[-first]- DCN[k]))
		ifelse(first==second,second<-(second+1),second<-second)		#as we remove the point identified as 'first' from the colsest point test, if second==first it means second =first+1 of the initial point list
		proxima[k,2] <- second						#second closest point
	}
proxima<-proxima+1 																						# as the first point do not appear in the cordal distance vector
proxima2 <- matrix(nrow=req,ncol=2)
for (i in 1:req)
	{
		if(proxima[i,1]>proxima[i,2])
		{
		proxima2[i,1]<-proxima[i,2];proxima2[i,2]<-proxima[i,1]}
		else if (proxima[i,1]<proxima[i,2]){
		proxima2[i,1]<-proxima[i,1];proxima2[i,2]<-proxima[i,2]}
	}																														#reordering the two closest points in order to get the one with the lower index first
VEC<-matrix( nrow=req, ncol = 3)															#VEC=vector from proximal point [n,1] to the proximal point [n,2]
for(l in 1:req)
	{
		VEC[l,]<-as.matrix(cur[proxima2[l,2],]-cur[proxima2[l,1],])
	}
COMP<-NULL																										#COMP= distance between the new point and the closest old point[n,i]
for(n in 1:req)
	{
		COMP[n]<-DCN[n]-DCO[proxima2[n,1]-1]											#as DCO length is p-1 (p=number of points)
	}
NOR<-NULL  																										#transformation applied to the vector
for(m in 1:req)
	{
		NOR[m]<-COMP[m]/(DPO[proxima2[m,1]])
	}
VECF<-VEC*NOR
PTS<-cur[proxima2[,1],]+VECF
PTS<-rbind(cur[1,],PTS,cur[dim(cur)[1],])
PTS
}

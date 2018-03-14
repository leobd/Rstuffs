#############################################################################################
## Rewritting of a part of Tristan Stayton's 'convrat' R function from the 'convevol' library
## Workhorse for the convratparr function
#############################################################################################

uniconvrat<-function(phyl,phentot,convtips)
{
##recup des index numeriques sur l'axe
conv1<-convtips[1]
conv2<-convtips[2]
ifelse(is.character(conv1)==T,tipvalue1<-which(phyl[["tip.label"]]==conv1),tipvalue1<-conv1)
ifelse(is.character(conv2)==T,tipvalue2<-which(phyl[["tip.label"]]==conv2),tipvalue2<-conv2)

##calcul lignées
mrcatips<-getMRCA(phyl, convtips)

	i=tipvalue1
	lineage1<-tipvalue1
	while(i!=mrcatips)
		{
			ancestor<-phyl$edge[which(phyl$edge[,2]==i),1]
			lineage1<-c(lineage1,ancestor)
			i<-ancestor
		}

	j=tipvalue2
	lineage2<-tipvalue2
	while(j!=mrcatips)
		{
			ancestor<-phyl$edge[which(phyl$edge[,2]==j),1]
			lineage2<-c(lineage2,ancestor)
			j<-ancestor
		}
		
##calcul des distances entre tt les nodes
	dt<-dist(phentot[unique(c(lineage1,lineage2)),])

##recup valeurs max
	maxval<-dt[which.max(dt)]
	tipdist<-dist(phentot[convtips,])

	C1<-1-(tipdist/maxval)
	C2<-maxval-tipdist
##phendist in branches along lineages
distlineage1<-0
for (i in 1:(length(lineage1)-1))
	{
	distlineage1<-distlineage1+dist(phentot[c(lineage1[i],lineage1[i+1]),])
	}
distlineage2<-0
for (i in 1:(length(lineage2)-1))
	{
	distlineage2<-distlineage2+dist(phentot[c(lineage2[i],lineage2[i+1]),])
	}


C3<-C2/(distlineage1+distlineage2)

##phendist totale dans le sous arbre definit par le MRCA

	getDescendants<-function(tree,node,curr=NULL){				##Fonction tirée du blog de Revell
	if(is.null(curr)) curr<-vector()
	daughters<-tree$edge[which(tree$edge[,1]==node),2]
	curr<-c(curr,daughters)
	w<-which(daughters>=length(tree$tip))
	if(length(w)>0) for(i in 1:length(w))
	curr<-getDescendants(tree,daughters[w[i]],curr)
	return(curr)
	}

subtr<-getDescendants(phyl,mrcatips)

a<-list()
for (i in 1:length(subtr))		##ça vas etre MOCHE >:D
	{
	a[[i]]<-(which(phyl[["edge"]][,1]==subtr[i]|phyl[["edge"]][,2]==subtr[i]))
	}
phy2dist<-unique(unlist(a))

## calcul de la distance totale dans le sous arbre defini par le MRCA, toute les branches, y compris celles hors ligné tip-MRCA 
distsub<-NULL
for(i in 1:length(phy2dist))
	{
	distsub[i]<-dist(rbind(phentot[phyl$edge[phy2dist,][i,1],],phentot[phyl$edge[phy2dist,][i,2],]))
	}
totalchanges<-sum(distsub)

C4<-C2/totalchanges

output<-c(C1,C2,C3,C4)
names(output)<-c("C1","C2","C3","C4")
output

}

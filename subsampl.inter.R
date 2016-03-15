############################################################################
##            EXTENDING 'cursub.interpo.R' TO A LIST OF CURVES            ##
############################################################################

subsampl.inter<-function(matlm,curlist,required,fix)
		{
		if (is.list(curlist)==F)
		print("curlist must be a list giving the curves(rowindex)")
		else 
		if (is.vector(required)==F)
		print("required must be a vector giving the number of points required per curve")
		else 
		if (length(curlist)!=length(required))
		print("curlist and required must be of same length")
		else 
		
		output<-matlm[fix,]
			for (i in 1:length(curlist))
				{
				cur<-matlm[curlist[[i]],]
				req<-required[i]
				out<-cursub.interpo(cur,req)
				rownames(out)<-paste("curve",i,"-",(1:dim(out)[1]-1),sep="")
				output<-rbind(output,out[2:(dim(out)[1]-1),])
				}
			output	
		}

##ExtractGM() - Extract the CIs from capture.output(GM) - Goodman's multinomial simultaneous CI function.  Weird way but only way
##              I could extract the values.  Below is an example of what capture.output(GM()) returns.

# [1] "Original Intervals"                "Lower Limit"                      
# [3] "[1] 0.3278298 0.3619442 0.1314142" "Upper Limit"                      
# [5] "[1] 0.4622324 0.4980913 0.2374017" "Adjusted Intervals"               
# [7] "Lower Limit"                       "[1] 0.3278298 0.3619442 0.1314142"
# [9] "Upper Limit"                       "[1] 0.4622324 0.4980913 0.2374017"
# [11] "Volume"                            "[1] 0.00193941"   


ExtractGM<-function(b)		## b is y[3] or y[5]
{
    leny<-nchar(b)
    begin<-0
    end<-0
    ci.df<-NULL
    for(i in 1:leny) {
        c<-substr(b,i,i)
        if(c==" "){		## Find blanks.  Beginning of number is +1, end of number is -1 from i.
          if(begin==0) {
             begin<-i+1
	  } else {
            end<-i-1
            d<-substr(b,begin,end)
            ci.df<-rbind(ci.df,as.numeric(d))
            begin<-i+1
            end<-0
          }
       }
    }
    d<-substr(b,begin,leny)		## The last number has no blank to the RHS, thus use total length
    ci.df<-rbind(ci.df,as.numeric(d))
    ci.df<-ci.df*100.0			## Convert to percentage value
    return(ci.df)
}
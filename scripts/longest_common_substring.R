# Find longest common substring
# by https://stackoverflow.com/users/4137985/cath

comsub<-function(x) {
    # sort the vector
    x<-sort(x)
    # split the first and last element by character
    d_x<-strsplit(x[c(1,length(x))],"")
    # search for the first not common element and so, get the last matching one
    der_com<-match(FALSE,do.call("==",d_x))-1
    # if there is no matching element, return an empty vector, else return the common part
    ifelse(der_com==0,return(character(0)),return(substr(x[1],1,der_com)))
}

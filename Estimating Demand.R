# Model is described in:  https://www0.gsb.columbia.edu/mygsb/faculty/research/pubfiles/5397/vanRyzin_Estimating.pdf
dat = matrix(
  c(10,15,11,14, rep(NA, times=11),
    11,6,11,8,20,16, rep(NA, times = 9),
    5,6,1,11,4,5,14,7,11, rep(NA, times=6),
    4,4,4,1,6,4,3,5,9,9,6,9, rep(NA, times=3),
    0,2,0,0,1,0,1,3,0,3,3,5,2,3,3), nrow=5, ncol=15, byrow = T)

(v=c(1,.7,.4,.2,.05))
s=0.7  #market share
r= (1-s)/s   #attractiveness of alternatives combined



T= dim(dat)[2] #time periods
n = dim(dat)[1] #number of products

# Initialise N0, N1,...Nn
x=dat #primary demand
x[is.na(x)] = 0

y= matrix(NA, nrow=n, ncol=T) #substitute demand
N = apply(x, 1, sum) #Total purchases of j projects
N0 = r*sum(N)  #Probability of no purchase
x0 = rep(N0/T, times=T)

v=N/N0  #Initial est of preferences = Total sales/Prob of no purchase
#ll = numeric()
y0=numeric()
niter = 1
epsilon = 0.001 #convergence condition
lambda = numeric(T) # customer arrival rate
x_pre = matrix(NA, nrow=n, ncol=T)

em= function(dat, epsilon=0.001){
  repeat{
    # E-step
    
    for (t in seq_len(T)){
      
      # Identify which j in St
      St= which(!is.na(dat[,t]))
      
      for (j in seq_len(n)){
        if(is.na(dat[j,t])){ #if j is not in St
          x[j,t]= (v[j]/(sum(v)+1))*((sum(v[St])+1)/sum(v[St]))*sum(dat[St,t])
          y[j,t]=-x[j,t]
        }
        else{ #if j in St
          y[j,t]= (sum(c(v[-St]))/(sum(v)+1))*dat[j,t]
          x[j,t]=dat[j,t]-y[j,t]
        }
      }
      x0[t]=(1/sum(v))*sum(x[,t])
      y0[t]=(1/(sum(v[St])+1))*sum(x[-St, t])
      lambda[t] = x0[t]+sum(x[,t])
    }
    # M-step
    N0 = sum(x0)
    for (j in seq_len(n)){
      N[j]=sum(x[j,])
      v[j]=N[j]/N0
    }
    #ll.calc = numeric(n)
    #for (j in seq_len(n)){
    #  ll.calc[j]=N[j]*log(v[j]/(sum(v)+1))
    #}
    #ll = c(ll, sum(ll.calc)+r*log(1/(sum(v)+1))*sum(N))
    print(niter)
    # Numerical Convergence condition
    # Stop iterating difference between all values Xjt from
    # two consecutive iterations of the algorithm is less than epsilon
    if(sum(abs(x-x_pre)<=epsilon, na.rm=T)== n*T){ 
      break
    }
    else{
      niter = niter+1
      x_pre=x
    }
  }
  return(list(v=v, N=N, y=y, x=x, niter=niter, y0=y0, x0=x0, lambda=lambda))
}
em = em(dat, epsilon=0.001)

# Percentage of lost sales
sum(em$y0)/sum(em$N)

####
em= function(dat, epsilon=0.001){
  repeat{
    # E-step
    
    for (t in seq_len(T)){
      
      # Identify which j in St
      St= which(!is.na(dat[,t]))
      
      for (j in seq_len(n)){
        if(is.na(dat[j,t])){ #if j is not in St
          x[j,t]= (v[j]/(sum(v)+1))*((sum(v[St])+1)/sum(v[St]))*sum(dat[St,t])
          y[j,t]=-x[j,t]
        }
        else{ #if j in St
          y[j,t]= (sum(c(v[-St]))/(sum(v)+1))*dat[j,t]
          x[j,t]=dat[j,t]-y[j,t]
        }
      }
      x0[t]=(1/sum(v))*sum(x[,t])
      y0[t]=(1/(sum(v[St])+1))*sum(x[-St, t])
      lambda[t] = x0[t]+sum(x[,t])
    }
    # M-step
    N0 = sum(x0)
    for (j in seq_len(n)){
      N[j]=sum(x[j,])
      v[j]=N[j]/N0
    }
    #ll.calc = numeric(n)
    #for (j in seq_len(n)){
    #  ll.calc[j]=N[j]*log(v[j]/(sum(v)+1))
    #}
    #ll = c(ll, sum(ll.calc)+r*log(1/(sum(v)+1))*sum(N))
    print(niter)
    # Numerical Convergence condition
    # Stop iterating difference between all values Xjt from
    # two consecutive iterations of the algorithm is less than epsilon
    if(sum(abs(x-x_pre)<=epsilon, na.rm=T)== n*T){ 
      break
    }
    else{
      niter = niter+1
      x_pre=x
    }
  }
  
  return(list(v_est_pref=v, N_est_tot_demand=N, y_sub_demand=y, 
              x_true_demand=x, n_iterations=niter, y0_est_netloss_perday=y0, 
              x0_est_nopurch=x0, lambda_est_visits=lambda))

}
em = em(dat, epsilon=0.001)

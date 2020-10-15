// Create a file to run the TMB version of sprat
#include <TMB.hpp>
#include <iostream>

template <class Type>
vector<Type> cumsum(vector<Type> x) {
  int n = x.size();
  vector<Type> ans(n);
  ans[0] = x[0];
  for (int i = 1; i < n; i++) ans[i] = x[i] + ans[i-1];
  return ans;
}


template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data input
  DATA_ARRAY(wage_ssb); // Weight in the beginning of the year
  DATA_ARRAY(wage_catch); // Weight in catch
  DATA_ARRAY(wage_survey); // Weight in survey
  DATA_ARRAY(wage_mid); // Weight in the middle of the year
// //
// // // Age
  DATA_INTEGER(n_age); // Plus group
  DATA_INTEGER(sum_zero); // should rec dev's sum so zero?
  DATA_VECTOR(ages); // ages
  DATA_INTEGER(t_end);
  DATA_INTEGER(yr_sel);
  DATA_INTEGER(sel_change_yr);
  DATA_SCALAR(log_q);
  DATA_VECTOR(b); // bias adjustment factor
  DATA_VECTOR(yrs);
  DATA_VECTOR(flag_sel);
// // Selectivity
  DATA_INTEGER(s_min);
  DATA_INTEGER(s_min_survey);
  DATA_INTEGER(s_max);
  DATA_INTEGER(s_max_survey);
// // // Survey
  DATA_VECTOR(survey); // Acoustic survey
  //DATA_VECTOR(survey_x); // Flag if survey occurred - Replaced with flag_survey (only one instance in code)
  DATA_VECTOR(survey_err); // - TODO: numbers different
  DATA_VECTOR(ss_survey); // Age comp sample size
  DATA_VECTOR(flag_survey); // Were ages sampled this year

// TODO: fix age_survey - need age 1-15 and replace NA with -1
  DATA_ARRAY(age_survey); // Age compositions
  DATA_INTEGER(age_max_age); // Last age included in age comps
  DATA_SCALAR(s_mul); // Multiplier for survey selectivity
 // Catches
  DATA_VECTOR(catch_obs); // Total catch
  DATA_VECTOR(ss_catch); // age comp sample size
  DATA_VECTOR(flag_catch); // Years age was sampled
  DATA_ARRAY(age_catch); // Age comps

  DATA_SCALAR(log_sd_catch); // Error on catch
  DATA_SCALAR(rdev_sd); // Can it be estimated as a fixed effect?
  DATA_SCALAR(sigma_p_sel); // selectivity SD

//
//   // Mortality
  DATA_VECTOR(m_sel); // How mortality scales with age
  DATA_VECTOR(mat_sel); // Maturity ogive
  // Priors
  DATA_SCALAR(b_prior);
  DATA_SCALAR(a_prior);
//   // Time parameters
  // Parameter integers
  PARAMETER(log_r_init); // Recruitment at
  PARAMETER(log_h); // Steepness
  PARAMETER(log_m_init); // Natural mortality
  PARAMETER(log_sd_surv); // Survey uncertainty
  //PARAMETER(rdev_sd);

  PARAMETER(log_phi_catch);
  // PARAMETER(log_phi_survey);
  DATA_SCALAR(log_phi_survey);
  PARAMETER_VECTOR(p_sel_fish);
  PARAMETER_VECTOR(p_sel_surv);
  PARAMETER_VECTOR(init_n);
  PARAMETER_VECTOR(r_in); // Time varying stuff
  PARAMETER_ARRAY(p_sel); // Time varying selectivity
  PARAMETER_VECTOR(f_0);

  // Transform out of log space
  Type SDsurv = exp(log_sd_surv);
  Type SDcatch = exp(log_sd_catch);
  Type SDR = exp(rdev_sd);
  Type r_init = exp(log_r_init);
  Type h = exp(log_h);
  Type Minit = exp(log_m_init);
  Type q = exp(log_q);
  Type phi_survey = exp(log_phi_survey);
  Type phi_catch = exp(log_phi_catch);

//
// //  Minor calculations
  vector<Type> M = Minit*m_sel; // Natural mortality
  vector<Type> logF(t_end);
  vector<Type> logR(t_end);
//
  // Vectors for saving stuff
  vector<Type>R(t_end);
  array<Type> CatchAge(n_age,t_end);
  array<Type> CatchNAge(n_age,t_end);
//


for(int j=0;j<(t_end-1);j++){
      logR(j)=r_in(j);
}
logR(t_end-1) = 0;

// selectivity
// survey
vector<Type>surveyselc(n_age);
Type pmax = sum(p_sel_surv);
Type ptmp = 0;

for(int j=0;j<n_age;j++){ // Fix the survey selectivity
  if (ages(j) < s_min_survey){
    surveyselc(j) = 0;
  }
   if (ages(j) == s_min_survey){
    ptmp = 0;
    surveyselc(j) = exp(ptmp-pmax);
  }
   if ((ages(j) > s_min_survey) & (ages(j) <= s_max_survey)){
     ptmp = p_sel_surv(j-s_min_survey-1)+ptmp;
     surveyselc(j) = exp(ptmp-pmax);
   }
   if(ages(j) > (s_max_survey)){
    surveyselc(j) = surveyselc(s_max_survey);
  }
}
// Fishing mortality
vector<Type> catchselec(n_age);
Type pmax_catch = max(cumsum(p_sel_fish));
Type ptmp_catch = 0;
//
for(int time=0;time<t_end;time++){ // Start time loop
    for (int j=0;j<n_age;j++){
      if (ages(j) < s_min){
          catchselec(j) = 0;
      }
        if (ages(j) == s_min){
          ptmp_catch = 0;
          catchselec(j) = exp(ptmp_catch-pmax_catch);
      }
        if ((ages(j) > s_min) & (ages(j) <= s_max)){
          ptmp_catch = p_sel_fish(j-s_min-1)+ptmp_catch;
          catchselec(j) = exp(ptmp_catch-pmax_catch);
      }
        if(ages(j) > (s_max_survey)){
          catchselec(j) = catchselec(s_max);
      }
    }
  }

vector<Type> Nzero(n_age); // Numbers with no fishing
//vector<Type>Meq = cumsum(M);
//
Nzero(0) = r_init;
for(int i=1;i<(n_age-1);i++){
    Nzero(i) = r_init * exp(-(M(i)*ages(i)));
  }
//
// Nzero(n_age-1) = r_init*exp(-(M(n_age-2)*age(n_age-2)))/(Type(1.0)-exp(-M(n_age-1)));//*exp(init_n(n_age-2)); // Plus group

Nzero(n_age-1) = (r_init*exp(-(M(n_age-2)*ages(n_age-1))))/(Type(1.0)-exp(-M(n_age-1)));

array<Type> SSBage(n_age);
array<Type> Catchinit(n_age);
array<Type>selectivity_save(n_age,t_end);
Type SSBzero = 0;
vector<Type> Zzero = M;

for(int i=0;i<n_age;i++){ // Loop over ages
    SSBzero += mat_sel(i)*Nzero(i)*0.5;
  }
// Run the initial distribution
 REPORT(SSBzero);

// Type SSBinit = 0;
//
// // for(int i=0;i<(n_age);i++){ // Loop over other ages
// //     Ninit(i,0) = Nzero(i);
// //   }
// Ninit(0) = r_init;
// for(int i=1;i<(n_age-1);i++){
//     Ninit(i) = r_init * exp(-(M(i)*age(i)))*exp(-0.5*0*SDR*SDR+init_n(i-1));
//   }
// //
// Ninit(n_age-1) = r_init*exp(-(M(n_age-1)*age(n_age-1)))/(Type(1.0)-exp(-M(n_age-1)))*exp(-0.5*0*SDR*SDR+init_n(n_age-2));
//
// for(int i=0;i<(n_age);i++){ // Loop over other ages
//   SSBinit += Ninit(i)*mat_sel(i)*0.5;
// }

// Plus group
vector<Type>Catch(t_end);
vector<Type>CatchN(t_end);
array<Type> N_beg(n_age,t_end+1);
array<Type> N_mid(n_age,t_end+1);
N_beg.setZero();
N_mid.setZero();
// }
// // Run the model over time
array<Type> SSB(t_end);
array<Type>Surveyobs(t_end); // Survey observed Surveyobs
array<Type>Surveyobs_tot(t_end); // Total Surveyobs over age 2
array<Type>age_survey_est(age_max_age,t_end);
array<Type>age_catch_est(age_max_age,t_end);
array<Type>Zsave(n_age,t_end);
//
age_survey_est.setZero();
age_catch_est.setZero();
Catch.setZero();
CatchN.setZero();
vector<Type> Myear = M*m_sel; // Natural mortality (if we want to change it later)
//
vector<Type> Fyear(t_end);
vector<Type> Freal(n_age);
vector<Type> Z(n_age);
vector<Type>pmax_catch_save(t_end);
vector<Type>p_sel_fish_zero = p_sel_fish;
vector<Type>Catchsave(t_end);
//vector<Type>Fpope(t_end);

// vector<Type>Bmid(n_age); // Biomass at the middle of the year
// // vector<Type>test(3);
// // test = cumsum(test);
// REPORT(test)
//array<Type> p_sel_save(5,)

for(int time=0;time<(t_end);time++){ // Start time loop

    Type Ntot_survey = 0;
    pmax_catch_save(time) = pmax_catch;
    // Take care of selectivity
    REPORT(flag_sel)
    REPORT(p_sel.cols())

    if (flag_sel(time) == 1){

           for(int i=0;i<p_sel_fish.size();i++){
           p_sel_fish(i) = p_sel_fish_zero(i)+p_sel(i,time-sel_change_yr+1)*sigma_p_sel; // 27 is the number of years selectivity is calculated p_sel.cols()-1/ time-sel_change_yr-
           }

           pmax_catch = max(cumsum((p_sel_fish)));
           pmax_catch_save(time) = pmax_catch;

           for(int j=0;j<(n_age);j++){ // Fix the Catch selectivity
               if (ages(j) == s_min){
                 ptmp_catch = 0;
                 catchselec(j) = exp(ptmp_catch-pmax_catch);
             }
               if ((ages(j) > s_min) & (ages(j) <= s_max)){
                 ptmp_catch = p_sel_fish(j-s_min-1)+ptmp_catch;
                 catchselec(j) = exp(ptmp_catch-pmax_catch);
             }
               if(ages(j) > (s_max_survey)){
                 catchselec(j) = catchselec(s_max);
             }
           }
    }


       Catch(time) = 0;

       Fyear(time) = f_0(time);



    if (time == 0){
      for(int i=1;i<(n_age-1);i++){
        N_beg(i,time) = r_init * exp(-0.5*0*SDR*SDR+init_n(i-1))*exp(-Myear(i)*ages(i));
      }
        N_beg(n_age-1, time) = r_init * exp(-0.5*0*SDR*SDR+init_n(n_age-2)) * exp(-Myear(n_age-1) * ages(n_age-1)) / (1 - exp(-Myear(n_age-1)));

    }

    for(int i=0;i<n_age;i++){ // Loop over other ages
         SSB(time) += N_beg(i,time)*wage_ssb(i,time)*0.5; // hat
         //SSB(time) += N_beg(i,time)*mat_sel(i); // hat

      }

    for(int i=0;i<(n_age);i++){ // Loop over other ages
          Freal(i) = Fyear(time)*catchselec(i);
          Z(i) = Freal(i)+Myear(i);
          selectivity_save(i,time) = catchselec(i);
          Zsave(i,time) = Z(i);


     }

    R(time) = (4*h*r_init*SSB(time)/(SSBzero*(1-h)+ SSB(time)*(5*h-1)))*exp(-0.5*b(time)*SDR*SDR+logR(time));
    N_beg(0,time) = R(time); // First one is recruits
    //

    //Type s_mul = Type(0.58);

    for(int i=0;i<(n_age-1);i++){ // Loop over other ages
    N_mid(i,time) = N_beg(i,time)*exp(-Z(i)*s_mul);
    N_beg(i+1,time+1) = N_beg(i,time)*exp(-Z(i));
    }
    // // Plus group
    N_mid(n_age-1, time) = N_beg(n_age-2,time)*exp(-Z(n_age-2)*0.5)+N_beg(n_age-1,time)*exp(-Z(n_age-1)*s_mul);
    N_beg(n_age-1, time+1) = N_beg(n_age-2,time)*exp(-Z(n_age-2))+N_beg(n_age-1,time)*exp(-Z(n_age-1));

    Catch(time) = 0;

    for(int i=0;i<n_age;i++){ // Loop over other ages
        CatchAge(i,time)= (Freal(i)/(Z(i)))*(1-exp(-Z(i)))*N_beg(i,time)*wage_catch(i,time);// Calculate the catch in kg
        CatchNAge(i,time)= (Freal(i)/(Z(i)))*(1-exp(-Z(i)))*N_beg(i,time);// Calculate the catch in kg
        Catch(time) += CatchAge(i,time);
        CatchN(time) += CatchNAge(i,time);

        Surveyobs(time) += surveyselc(i)*wage_survey(i,time)*N_mid(i,time)*q;
        Ntot_survey += surveyselc(i)*N_mid(i,time); // To use with age comps
      }


      if(flag_survey(time) == 1){ // Flag if  there was a measurement that year

        for(int i=0;i<(n_age-1);i++){ // Loop over other ages
          if(i < age_max_age){
          age_survey_est(i,time) = (surveyselc(i+1)*N_mid(i+1,time))/Ntot_survey;
          }else{
          age_survey_est(age_max_age-1,time) += (surveyselc(i+1)*N_mid(i+1,time))/Ntot_survey;
          }
        }
      }  //Recruitment

    if(flag_catch(time) == 1){ // Flag if  there was a measurement that year

    for(int i=0;i<(n_age-1);i++){ // Loop over ages for catch comp
      if(i<age_max_age){
        age_catch_est(i,time) = (CatchNAge(i+1,time)/CatchN(time)); // Catch comp (1 bc the data starts at age = 1)
      }else{
        age_catch_est(age_max_age-1,time) += (CatchNAge(i+1,time)/CatchN(time));
       }
     }
   }
 }


// // // Make the observation model
// using namespace density;
Type ans_survey=0.0;
////Save the observation model estimates
for(int time=1;time<t_end;time++){ // Survey Surveyobs
// Used to be survey_x
      if(flag_survey(time) == 1){
        ans_survey += -dnorm(log(Surveyobs(time)), log(survey(time)), SDsurv+survey_err(time), TRUE);
    }
  }

Type ans_catch = 0.0;
for(int time=0;time<t_end;time++){ // Total Catches
        ans_catch += -dnorm(log(Catch(time)+1e-6), log(catch_obs(time)+1e-6), SDcatch, TRUE);
}

////Likelihood function for age composition in survey
//
Type ans_survcomp = 0.0;
Type ans_catchcomp = 0.0;


vector<Type>sum1(t_end);
vector<Type>sum2(t_end);

sum1.setZero();
sum2.setZero();


for(int time=1;time<t_end;time++){ // Loop over available years
        if(flag_survey(time) == 1){ // Flag if  there was a measurement that year
        for(int i=1;i<age_max_age;i++){ // Loop over other ages (first one is empty for survey)
          sum1(time) += lgamma(ss_survey(time)*age_survey(i,time)+1);
          sum2(time) += lgamma(ss_survey(time)*age_survey(i,time) + phi_survey*ss_survey(time)*age_survey_est(i,time)) - lgamma(phi_survey*ss_survey(time)*age_survey_est(i,time));
        }
        ans_survcomp += lgamma(ss_survey(time)+1)-sum1(time)+lgamma(phi_survey*ss_survey(time))-lgamma(ss_survey(time)+phi_survey*ss_survey(time))+sum2(time);
      }

}


vector<Type>sum3(t_end);
vector<Type>sum4(t_end);
//
sum3.setZero();
sum4.setZero();

for(int time=1;time<t_end;time++){ // Loop over available years
  if(Catch(time)>0){

        if(flag_catch(time) == 1){ // Flag if  there was a measurement that year
        for(int i=0;i<age_max_age;i++){ // Loop over other ages (first one is empty for survey)
          sum3(time) += lgamma(ss_catch(time)*age_catch(i,time)+1);
          sum4(time) += lgamma(ss_catch(time)*age_catch(i,time) + phi_catch*ss_catch(time)*age_catch_est(i,time)) - lgamma(phi_catch*ss_catch(time)*age_catch_est(i,time));
        }
        ans_catchcomp += lgamma(ss_catch(time)+1)-sum3(time)+lgamma(phi_catch*ss_catch(time))-lgamma(ss_catch(time)+phi_catch*ss_catch(time))+sum4(time);
      }
        }
}


Type ans_SDR = 0.0;

 for(int time=0;time<(t_end-1);time++){ // Start time loop
   ans_SDR += Type(0.5)*(logR(time)*logR(time))/(SDR*SDR)+b(time)*log(SDR*SDR);
 }





// Error for Selectivity
Type ans_psel = 0.0;
//
for(int time=0;time<yr_sel;time++){ // Start time loop
  for(int i=0;i<p_sel_fish.size();i++){ // Start time loop
        ans_psel += Type(0.5)*(p_sel(i,time)*p_sel(i,time))/(sigma_p_sel*sigma_p_sel);
      }
}

// Priors on h and M
Type ans_priors = 0.0;

for(int time=0;time<(n_age-1);time++){ // Start time loop
  ans_priors += Type(0.5)*(init_n(time)*init_n(time))/(SDR*SDR);
}

// ans_priors += -dnorm(log_h,log(Type(0.777)),Type(0.113),TRUE);

// Prior on h
ans_priors += -dbeta(h,b_prior,a_prior,TRUE);

if(sum_zero == 1){
  ans_priors += ((Type(0.0)-sum(logR))*(Type(0.0)-sum(logR)))/Type(0.01);
}

// ans_priors += -dnorm(log_m_init, log(Type(0.2)), Type(0.1), TRUE);
ans_priors += 0.5*pow(log_m_init-log(Type(0.2)),2)/Type(0.01);

Type ans = ans_SDR+ans_psel+ans_catch+ans_survey-ans_survcomp-ans_catchcomp+ans_priors;

// Later Fix F in the likelihood and age comp in catch
// Type ans = 0.0;
// Report calculations
ADREPORT(SSB)
//ADREPORT(N)
ADREPORT(Catch)
ADREPORT(logF)
ADREPORT(R)
ADREPORT(Surveyobs)
ADREPORT(Fyear)
ADREPORT(surveyselc)
ADREPORT(catchselec)
ADREPORT(age_catch)
ADREPORT(age_catch_est)
ADREPORT(age_survey)
ADREPORT(age_survey_est)
ADREPORT(SSBzero)

REPORT(SSB)
REPORT(Fyear)
REPORT(Catch)
REPORT(R)
REPORT(Nzero)
REPORT(Zsave)
REPORT(age_survey_est)
REPORT(age_catch_est)
REPORT(CatchN)
REPORT(selectivity_save)
REPORT(surveyselc)
REPORT(catchselec)
REPORT(N_beg)
REPORT(N_mid)
REPORT(Surveyobs)
REPORT(Myear)
REPORT(pmax_catch_save)

REPORT(ans_SDR)
REPORT(ans_psel)
REPORT(ans_catch)
REPORT(ans_survey)
REPORT(ans_survcomp)
REPORT(ans_catchcomp)
REPORT(ans_priors)
REPORT(age_survey_est)
REPORT(ans)

  return ans;
}

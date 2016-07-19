#include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
#include <unordered_map>
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp ;

// [[Rcpp::export]]
arma::sp_mat fcm_cpp(Rcpp::List &texts,
                    const CharacterVector &types,
                    const String &count,
                    const int &window,
                    const NumericVector &weights,
                    const bool &ordered) {
    int n = types.size();
    arma::sp_mat aFcm(n,n);
    std::unordered_map<String, int> id;
    for (int g = 0; g < types.size(); g++) {
        id[types[g]] = g ;                      
    }
  
    //int k = 0;
    double xValue;
    for (int h = 0; h < texts.size(); h++) {
        StringVector text = texts[h];
        int len = text.size();
        for (int i = 0; i < text.size(); i++) {
            int id_i = id[text[i]];
            int j_int = i+1;//std::max(0, i - window);
            int j_lim = std::min(i + window + 1, len);
            for(int j = j_int; j < j_lim; j++) {
                
                if (count == "frequency" || count == "boolean"){
                    xValue = 1;
                }else if(count == "weighted"){ 
                    if (weights.size() == 1){
                        xValue = 1.0/(j-i);  // default, weights=1/offset
                    }else{
                        xValue = weights[j-j_int];
                    }
                }
                
                
                int id_j = id[text[j]];
                if (ordered){
                    aFcm(id_i,id_j) += xValue;
                }else{
                    if (id_i<id_j){
                        aFcm(id_i,id_j) += xValue;
                    }else{
                        aFcm(id_j,id_i) += xValue;
                    }
                }
            }
        }
    }
    return aFcm;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

#fcm_cpp(rep(list(letters), 100), letters,"window", 5,1,TRUE)
*/

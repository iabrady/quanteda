#include <Rcpp.h>
#include <vector>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_map>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
List fcm_cpp(List &texts,
              const CharacterVector &types,
              const int &window,
              const int &n){
  
  IntegerVector index_tgt(n);
  IntegerVector index_col(n);
  std::unordered_map<String, int> id;
  for (int g = 0; g < types.size(); g++){
    id[types[g]] = g + 1;
  }
  int dim = id.size();
  //Rcpp::NumericMatrix fcm(dim, dim); // Make dim x dim matrix
  
  int k = 0;
  for (int h = 0; h < texts.size(); h++){
    StringVector text = texts[h];
    int len = text.size();
    for (int i = 0; i < text.size(); i++){
      int index_i = id[text[i]];
      int j_ini = std::max(0, i - window);
      int j_lim = std::min(i + window + 1, len);
      for(int j = j_ini; j < j_lim; j++){
        int index_j = id[text[j]];
        if(i==j) continue;
        //Rcout << k << " " << id[text[i]] << " " << id[text[j]] << "\n";
        index_tgt[k] = id[text[i]];
        index_col[k] = id[text[j]];
        k++;
      }
    }
  }
  return List::create(Named("target") = index_tgt[seq(0, k - 1)], 
                      Named("collocate") = index_col[seq(0, k - 1)]);
}




// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
fcm2_cpp(list(letters), letters, 5, 26 * (5 * 2))
#fcm2_cpp(rep(list(letters), 100), letters, 5, 26 * 100 * (5 * 2))
#fcm_cpp(rep(list(letters), 100), letters, 5)
*/

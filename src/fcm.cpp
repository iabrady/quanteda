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
NumericMatrix fcm_cpp(List &texts,
                      const CharacterVector &types,
                      const int &window){
  
  std::unordered_map<String, int> index;
  //index.reserve(types.size());
  for (int g = 0; g < types.size(); g++){
    index[types[g]] = g + 1;
  }
  int dim = index.size();
  Rcpp::NumericMatrix fcm(dim, dim); // Make dim x dim matrix
  
  for (int h = 0; h < texts.size(); h++){
    StringVector text = texts[h];
    int len = text.size();
    for (int i = 0; i < text.size(); i++){
      int index_i = index[text[i]];
      int j_ini = std::max(0, i - window);
      int j_lim = std::min(i + window + 1, len);
      for(int j = j_ini; j < j_lim; j++){
        int index_j = index[text[j]];
        fcm(index_i, index_j)++;
      }
    }
  }
  return fcm;
}




// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

fcm_cpp(rep(list(letters), 100), letters, 5)
*/

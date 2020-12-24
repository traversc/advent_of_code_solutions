#include <Rcpp.h>
#include <queue>
#include <cmath>
#include <unordered_set>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// https://stackoverflow.com/q/22280318/2723734
std::string print_queue(std::queue<int> q) {
  std::string output;
  while (!q.empty()) {
    output += std::to_string(q.front());
    output += " ";
    q.pop();
  }
  return output;
}

std::queue<int> subqueue(std::queue<int> q, size_t n) {
  std::queue<int> newq;
  for(size_t i=0; i<n; ++i) {
    newq.push(q.front());
    q.pop();
  }
  return newq;
}

int subgame(std::queue<int> v1, std::queue<int> v2, bool toplevel=false) {
  std::unordered_set<std::string> previous_states;
  while(true) {
    if(toplevel) {
      std::cout << "P1: " << print_queue(v1) << std::endl;
      std::cout << "P2: " << print_queue(v2) << std::endl;
    }
    int len1 = v1.size();
    int len2 = v2.size();
    if(len1 == 0) return 2;
    if(len2 == 0) return 1;
    
    std::string state = print_queue(v1) + "/ " + print_queue(v2);
    if(previous_states.find(state) != previous_states.end())  {
      std::cout << state << std::endl;
      return 1;
    }
    
    previous_states.insert(state);
    
    int card1 = v1.front(); v1.pop();
    int card2 = v2.front(); v2.pop();
    
    if(card1 < len1 && card2 < len2) {
      int res = subgame(subqueue(v1,card1), subqueue(v2,card2));
      if(res == 1) {
        v1.push(card1);
        v1.push(card2);
      } else {
        v2.push(card2);
        v2.push(card1);
      }
    } else {
      if(card1 > card2) {
        v1.push(card1);
        v1.push(card2);
      } else {
        v2.push(card2);
        v2.push(card1);
      }
    }
  }
}

// [[Rcpp::export(rng=false)]]
int run_game(std::vector<int> p1, std::vector<int> p2) {
  std::queue<int> v1;
  std::queue<int> v2;
  for(auto x : p1) v1.push(x);
  for(auto x : p2) v2.push(x);
  return subgame(v1, v2, true);
}
vector convolve(vector vec1, vector vec2) {
  int lvec1 = num_elements(vec1);
  int lvec2 = num_elements(vec2);
  vector[lvec1] conv = rep_vector(1e-8, lvec1);
  if (lvec2 > 1) {
    for (s in 1:lvec1) {
      conv[s] += dot_product(vec1[max(1, (s - lvec2 + 1)):s], 
                            tail(vec2, min(lvec2, s)));
    }
  }else{
    conv = vec1 * vec2[1];
  }
  return(conv);
}

real convolve_step(vector vec1, vector vec2, int index) {
  int lvec2 = num_elements(vec2);
  real conv = 1e-8;
  conv += dot_product(vec1[max(1, (index - lvec2 + 1)):index], 
                      tail(vec2, min(lvec2, index)));
  return(conv);
}
#include "cilkStars.h"

#include <stdio.h>
#include <stdlib.h>
#include <limits>

ppair cilk_friendly_stars(point *inp, int N) {
  // This is the serial version copied over.
  // TODO: Replace this with your parallel code!
  float min_distlist[N];
  ppair closest_pair[N];
  int index;
  
  cilk_for (int i=0; i<N; i++) {
    min_distlist[i] = std::numeric_limits<float>::infinity();
    for (int j=i+1; j<N; j++) {

      if(j != i) {
        point p = inp[i];
        point q = inp[j];

        float d = p.dist_squared(q);

        if (d < min_distlist[i]) {
          min_distlist[i] = d;
          closest_pair[i] = ppair(p, q);
        }

      }
    }
  }

  index = 0;
 
  for (int i = 0; i < N; i++) {
    if(min_distlist[i] < min_distlist[index]) {
      index = i;
    }
  }

  return closest_pair[index];
}

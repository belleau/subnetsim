subnetsim : Validating that a specific subnetwork has significantly more interactions than expected by randomness
====================

[![Build Status](https://travis-ci.org/belleau/subnetsim.svg?branch=master)](https://travis-ci.org/belleau/subnetsim)
[![codecov](https://codecov.io/gh/belleau/subnetsim/branch/master/graph/badge.svg)](https://codecov.io/gh/belleau/subnetsim)


The R **subnetsim** package implements a simulation analysis, based on bootstrapping, for testing the hypothesis that a selected subnetwork, which is part of a larger network, has more interactions than expected by randomness. To do so, subnetworks with the same number of nodes than the tested subnetwork are randomly generated by bootstrapping. Three measures of connectivity used to validate the hypothesis are: 

* the number of nodes in the subnetwork including all the first-degree neighbor nodes
* the number of links in the subnetwork including all the links of the first-degree neighbor nodes
* the number of links in the subnetwork

The R **subnetsim** package also offers the possibility to easily graph the results of the simulations.    
    
## Citing ##

If you use this package for a publication, we would ask you to cite the following:

> Martial Boutchueng-Djidjou, Pascal Belleau, Nicolas Bilodeau, Suzanne Fortier, Sylvie Bourassa, Arnaud Droit, Sabine Elowe, Robert L. Faure. 2018. A type 2 diabetes disease module with a high collective influence for Cdk2 and PTPLAD1 is localized in endosomes. bioRxiv doi: 10.1101/341693


## Authors ##

[Pascal Belleau](http://ca.linkedin.com/in/pascalbelleau "Pascal Belleau"),
[Astrid Desch&ecirc;nes](http://ca.linkedin.com/in/astriddeschenes "Astrid Desch&ecirc;nes"),
[Martial Boutchueng-Djidjou](https://www.researchgate.net/profile/Boutchueng_Djidjou_M),
[Robert L. Faure](http://www.crchudequebec.ulaval.ca/en/research/researchers/robert-l-faure/)

## License ##

This package and the underlying subnetsim code are distributed under 
the Artistic license 2.0. You are free to use and redistribute this software. 

For more information on Artistic 2.0 License see
[http://opensource.org/licenses/Artistic-2.0](http://opensource.org/licenses/Artistic-2.0)


## Bugs/Feature requests ##

If you have any bugs or feature requests, 
[let us know](https://github.com/belleau/subnetsim/issues). 

Thanks!

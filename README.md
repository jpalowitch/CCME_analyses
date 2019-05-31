## CCME analyses repo

This repository is meant to reproduce the experiments described in [Significance-based community detection in weighted networks (Palowitch et al. 2018)](http://www.jmlr.org/papers/v18/17-377.html).

To use, run the following from a Linux CLI or Mac terminal:

    git clone http://github.com/jpalowitch/CCME_analyses
    cd CCME_analyses
    bash sims-code/sbm_sims_script.txt
    bash applications-code/airports-script.txt
    bash applications-code/enron-script.txt
    
If you use code from this repo toward a publication, please cite the linked article above as: 

    @article{palowitch2017significance,
      title={Significance-based community detection in weighted networks},
      author={Palowitch, John and Bhamidi, Shankar and Nobel, Andrew B},
      journal={The Journal of Machine Learning Research},
      volume={18},
      number={1},
      pages={6899--6946},
      year={2017},
      publisher={JMLR.org}
    }
    

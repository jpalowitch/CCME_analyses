import sys
import numpy
from graph_tool.all import *
command_args = sys.argv
fn = 'sims-results/experiment6/90/1/network.csv'
g = load_graph_from_csv(fn, directed = False, 
                        eprop_types = ['double'], 
                        eprop_names = ['weight'])
N = g.num_vertices()
numpy.random.seed(12345)
state = minimize_blockmodel_dl(g, state_args = dict(recs=[g.ep.weight],
                                                    rec_types = ["real-exponential"]))
blocks = state.get_blocks()
sbm_clustering = [None] * N
for i in range(0, N):
  sbm_clustering[i] = blocks[i]
numpy.savetxt('sbm_clustering.dat', sbm_clustering)

import sys
import numpy
from graph_tool.all import *
command_args = sys.argv
# fn = command_args[1]
# g = load_graph(fn, directed = False)
g = load_graph_from_csv(fn, directed = False, 
                        eprop_types = ['double'], 
                        eprop_names = ['weight'])
N = g.num_vertices()
state = minimize_blockmodel_dl(g, state_args = dict(recs=[g.ep.weight],
                                                    rec_types = ["real-exponential"]))
blocks = state.get_blocks()
membership = [None] * N
for i in range(0, N):
  membership[i] = blocks[i]
numpy.savetxt(fname = command_args[2], X = membership)

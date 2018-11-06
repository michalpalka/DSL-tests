import pandas as pd
import numpy as np
import random
import json

import sys


# From Oskar's code (slightly changed)
def get_submatrix(mat, dimension, index):
  # Create a slice looking like 'mat[:,...,index,:,...]
  the_slice=tuple([slice(None, None, None) for d in range(0,dimension)] +
                  [index] +
                  [slice(None, None, None) for d in range(dimension+1, len(mat.shape))])
  # The submatrix is returned by reference
  return mat[the_slice]

def ipfNd(mat, margins, epsilon):
  dims = range(0, len(mat.shape)) #A list of all dimensions (0, 1, 2 ...)
  steps = 0

  diff_sum = 1.
  #print(mat)
  while diff_sum >= epsilon and steps < 100:
  #for k in range(0,5):
    diff_sum = 0.
    for dim in dims:
      axes = list(dims[:]) #Copy
      axes.remove(dim) #Remove current dimension
      sum_over_dims = mat.sum(axis=tuple(axes)) #Sum over all dimensions exept for for the current one

      diff_sum += sum(abs(sum_over_dims - margins[dim]))

      scalings = margins[dim]/sum_over_dims
      np.nan_to_num(scalings, copy=False)
      for scaling_i in range(0, len(scalings)):
        #Get a
        submat = get_submatrix(mat, dim, scaling_i)
        submat *= scalings[scaling_i]
    steps += 1
    print(diff_sum)
  return (mat, steps)

def ipfReport(mat, margins):
  dims = range(0, len(mat.shape)) #A list of all dimensions (0, 1, 2 ...)

  print('report:')

  for dim in dims:
    axes = list(dims[:]) #Copy
    axes.remove(dim) #Remove current dimension
    sum_over_dims = mat.sum(axis=tuple(axes)) #Sum over all dimensions exept for for the current one

    print(margins[dim])
    print(sum_over_dims)

def extract_index(tab, cols):
  if(len(cols) == 1):
    return tab[cols[0]]
  else:
    return tab[cols].apply(tuple, axis=1)

# Scale the marginals to make sure all tables have the same total sum
# (needed for convergence)
def normalise_marginals(marginals):
  if len(marginals) <= 1:
    return
  for i in range(1, len(marginals)):
    marginals[i] *= (sum(marginals[0]) / sum(marginals[i]))

# Compute the index of a multidimensional array stored
# in a 1-dimensional array. We need this, because we
# have a multidimensional array of vectors
def compute_index(ind, limits):
  pass

class IdMapping:
#  ID     = 0
#  ENUM   = 1
#  RANGES = 2
  def __init__(self, frm, to):
#    self.kind = ColMapping.ID
    self.frm = frm
    self.to = to
  def map(self, x):
    return x

class EnumMapping:
  def __init__(self, frm, to, r, default):
    self.frm = frm
    self.to = to
    self.payload = r
    self.default = default
  def map(self, x):
    for (c, lab) in self.payload:
      if x in c:
        return lab
    return self.default

class RangesMapping:
  def __init__(self, frm, to, r, default):
    self.frm = frm
    self.to = to
    self.payload = r
    self.default = default
  def map(self, x):
    for ((low, high), lab) in self.payload:
      if x >= low and x <= high:
        return lab
    return self.default

# handle default!
def make_mapping(j):
  jj = j['mapping']
  if jj['type'] == 'id':
    return IdMapping(j['from'], j['to'])
  if jj['type'] == 'enum':
    return EnumMapping(j['from'], j['to'], [(e['cases'], e['label']) for e in jj['enum']], None)
  if jj['type'] == 'ranges':
    return RangesMapping(j['from'], j['to'], [((e['range']['from'], e['range']['to']), e['label']) for e in jj['ranges']], None)


if len(sys.argv) != 2:
  print('Usage: python3 dslp_runtime.py <desc.json>', file=sys.stderr)
  exit(1)

with open(sys.argv[1]) as f:
    task_data = json.load(f)



marginals = []
m_columns = []
dimensions = []

for t in task_data['marginal_tables']:
  tab = pd.read_csv(t['table'], sep='\t')
  extract_index(tab, t['columns'])
  marginals.append(tab)
  m_columns.append(t['columns'])

micro_table = pd.read_csv(task_data['micro_data']['table'], sep='\t',  encoding='latin1')
micro_cols  = task_data['micro_data']['columns']
#micro_table = micro_table[micro_cols]

mappings = [make_mapping(m) for m in task_data['mappings']]

out_columns = task_data['output']['columns']

mat_shape = [t.shape[0] for t in marginals]


# Bucket
# First, create indices that map combinations of attributes to the
# marginal table rows
indices = []
for (mr, c) in zip(marginals, m_columns):
  index = {}
  for (i, r) in mr.iterrows():
    index[tuple([r[col] for col in c])] = i
  indices.append(index)

# Add seed
seed = np.zeros(np.product(mat_shape), dtype=np.float).reshape(mat_shape)
# Work around the zero problem by setting the zero cells to a small positive value
seed[:] += 1e-12

buckets = np.empty(np.product(mat_shape), dtype=object).reshape(mat_shape)
# We need this acrobatics to create an array of empty lists that are not shared
for i, x in np.ndenumerate(buckets):
  buckets[i] = list()
 
# Put microsample records into buckets
# We need to use tuples here to preserve types
for row in micro_table.itertuples():
  marginal_cell = {}
  # All relevant columns must be mentioned in mappings
  for m in mappings:
    # For some reason the column index reported by get_loc() is shifted by 1
    marginal_cell[m.to] = m.map(row[micro_table.columns.get_loc(m.frm)+1])
  inds = [ind[tuple([marginal_cell[col] for col in c])] for (c, ind) in zip(m_columns, indices)]
  seed[tuple(inds)] += 1
  buckets[tuple(inds)].append(row)

outfile = task_data['output']['file_name']

reg = task_data['marginal_region']

marginals_reg = [t[reg] for t in marginals]
normalise_marginals(marginals_reg)

seed2 = seed.copy()



(full_mat, iterations) = ipfNd(seed2, marginals_reg, 0.05)
#print(full_mat)
print('iterations: {0}'.format(iterations))

# The weights need to be normalised for sampling
full_mat_norm = full_mat.copy()
full_mat_norm /= full_mat_norm.sum()

# Sample and write to file
num_samples = 0
with open(outfile, mode='w') as out_h:
  # Print header
  # (not yet)

  for (i, c) in enumerate(out_columns):
    if i != 0:
      out_h.write('\t')
    out_h.write(c)
  out_h.write('\n')

  while True:
    # Select the bucket according to the weights
    x = np.random.choice(buckets.flatten(), p=full_mat_norm.flatten())
    # If the bucket is empty, we resample
    if not x:
      continue
    # Otherwise, sample uniformly from the bucket
    j = np.random.choice((0, len(x)-1))
    for (i, c) in enumerate(out_columns):
      if i != 0:
        out_h.write('\t')
      out_h.write(str(x[j][micro_table.columns.get_loc(c)+1]))
    out_h.write('\n')
    num_samples += 1
    if num_samples == 10:
      break


#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

@author: remstef
"""

from pyfasttext import FastText

class FastTextEmbedding(Embedding):

  def __init__(self, binfile, normalize = False):
    self.file = binfile
    self.vdim = -1
    self.normalize = normalize

  def load(self):
    print('Loading fasttext model.')
    self.ftmodel = FastText()
    self.ftmodel.load_model(self.file)
    self.vdim = len(self.ftmodel['is'])
    print('Finished loading fasttext model.')
    return self

  def getVector(self, word):
    return self.ftmodel.get_numpy_vector(word, normalized = self.normalize)

  def wordForVec(self, v):
    word, sim = self.ftmodel.words_for_vector(v)[0]
    return word, sim

  def nearest_neighbors(self, word, n=200):
    tuples = ftmodel.nearest_neighbors(word, n)
    return tuples

  def nearest_neighbors_by_vector(self, word, n=200):
    tuples = self.ftmodel.words_for_vector(v, n)
    return tuples

  def containsWord(self, word, explicit=False):
    if explicit:
      return word in vocabulary()
    return True

  def vocabulary(self):
    return self.ftmodel.words

  def dim(self):
    return self.vdim

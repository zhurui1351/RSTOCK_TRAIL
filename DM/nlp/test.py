# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import nltk
from nltk.book import *
from __future__ import division
#nltk.download()
text1.concordance("monstrous")
text1.similar("monstrous")
text2.common_contexts(["monstrous", "very"])
text4.dispersion_plot(["citizens", "democracy", "freedom", "duties", "America"])
len(text3)
sorted(set(text3))
len(set(text3))
len(text3) / len(set(text3))
text3.count("smote")
100 * text4.count('a') / len(text4)
def lexical_diversity(text):
    return len(text) / len(set(text))
    
def percentage(count, total):
    return 100 * count / total
lexical_diversity(text3)
percentage(4, 5)
sent1 = ['Call', 'me', 'Ishmael', '.']
sent1+sent4
sent1.append("Some")
text4[173]
text4.index('awaken')
#切片
text5[16715:16735]
text5[:100]
name = 'Monty'
name[0]
name * 2

' '.join(['Monty', 'Python'])
'Monty Python'.split()
saying = ['After', 'all', 'is', 'said', 'and', 'done','more', 'is', 'said', 'than', 'done']
tokens = set(saying)
tokens = sorted(tokens)
tokens[-2:]
#频率分布
fdist1 = FreqDist(text1)
vocabulary1 = fdist1.keys()
fdist1['whale']
fdist1.plot(50, cumulative=True)
fdist1.hapaxes()

V = set(text1)
long_words = [w for w in V if len(w) > 15]
fdist5 = FreqDist(text5)
sorted([w for w in set(text5) if len(w) > 7 and fdist5[w] > 7])
#搭配词和双连词
text4.collocations()

#长度分布
[len(w) for w in text1]
fdist = FreqDist([len(w) for w in text1])
fdist.items()
fdist.freq(3)
fdist.max()
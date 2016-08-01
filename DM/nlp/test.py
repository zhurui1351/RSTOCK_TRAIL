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

sorted([w for w in set(text1) if w.endswith('ableness')])
[w.upper() for w in text1]

for word in ['Call', 'me', 'Ishmael', '.']:
    print word

#获取语料库
nltk.corpus.gutenberg.fileids()
emma = nltk.corpus.gutenberg.words('austen-emma.txt')
emma = nltk.Text(nltk.corpus.gutenberg.words('austen-emma.txt'))
emma.concordance("surprize")
from nltk.corpus import gutenberg
gutenberg.fileids()

for fileid in gutenberg.fileids():
    num_chars = len(gutenberg.raw(fileid))
    num_words = len(gutenberg.words(fileid))
    num_sents = len(gutenberg.sents(fileid))
    num_vocab = len(set([w.lower() for w in gutenberg.words(fileid)]))
    print int(num_chars/num_words), int(num_words/num_sents), int(num_words/num_vocab), fileid

#句子划分
macbeth_sentences = gutenberg.sents('shakespeare-macbeth.txt')
longest_len = max([len(s) for s in macbeth_sentences])
#网络聊天语料库
from nltk.corpus import webtext
from nltk.corpus import nps_chat
chatroom = nps_chat.posts('10-19-20s_706posts.xml')
chatroom[123]
from nltk.corpus import brown
brown.categories()
brown.sents(categories=['news', 'editorial', 'reviews'])
news_text = brown.words(categories='news')
fdist = nltk.FreqDist([w.lower() for w in news_text])
modals = ['can', 'could', 'may', 'might', 'must', 'will']
for m in modals:
    print m + ':', fdist[m]


cfd = nltk.ConditionalFreqDist(
    (genre, word)
    for genre in brown.categories()
    for word in brown.words(categories=genre))
genres = ['news', 'religion', 'hobbies', 'science_fiction', 'romance', 'humor']
modals = ['can', 'could', 'may', 'might', 'must', 'will']
cfd.tabulate(conditions=genres, samples=modals)

#路透语料库

from nltk.corpus import reuters
reuters.fileids()
reuters.categories()

reuters.categories(['training/9865', 'training/9880'])
reuters.fileids(['barley', 'corn'])
reuters.words('training/9865')[:14]
reuters.words(['training/9865', 'training/9880'])
reuters.words(categories=['barley', 'corn'])

#演说语料库
from nltk.corpus import inaugural
inaugural.fileids()
#多国世界人权宣言
from nltk.corpus import udhr
languages = ['Chickasaw', 'English', 'German_Deutsch','Greenlandic_Inuktikut', 'Hungarian_Magyar', 'Ibibio_Efik']
cfd = nltk.ConditionalFreqDist(
    (lang, len(word))
    for lang in languages
    for word in udhr.words(lang + '-Latin1'))
        
cfd.plot(cumulative = True)
cfd.tabulate(conditions=['English', 'German_Deutsch'],samples=range(10), cumulative=True)
#条件频率分布
genre_word = [(genre, word) for genre in ['news', 'romance'] 
for word in brown.words(categories=genre)]

cfd = nltk.ConditionalFreqDist(genre_word)
cfd.conditions()
list(cfd['romance'])
cfd['romance']['could']

from nltk.corpus import inaugural
cfd = nltk.ConditionalFreqDist(
    (target, fileid[:4])
    for fileid in inaugural.fileids()
    for w in inaugural.words(fileid)
    for target in ['america', 'citizen']
    if w.lower().startswith(target))
#随机产生文本
def generate_model(cfdist, word, num=15):
    for i in range(num):
        print word,
        word = cfdist[word].max()
text = nltk.corpus.genesis.words('english-kjv.txt')
bigrams = nltk.bigrams(text)
cfd = nltk.ConditionalFreqDist(bigrams)
print cfd['living']
generate_model(cfd, 'living')

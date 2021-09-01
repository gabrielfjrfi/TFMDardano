import detectlanguage

detectlanguage.configuration.api_key = "554d9d4c0ce6a4330d0476bb5ddc2a35"

def detector_idioma(texto):
  idioma=detectlanguage.simple_detect(texto)
  return idioma



import sys
import re
import xml.etree.ElementTree as ET
import pandas as pd

reload(sys)
sys.setdefaultencoding('utf-8')

def read_emojis():
    """
    Reads the emojis from Emoji Sentiment Ranking paper.
    Calculates the score for each emoji and return it.
    """
    emojis = {}
    firstline = True
    
    with open('./catalan-sentiment-analysis-master/src/data/Emoji Sentiment Ranking 1.0/Emoji_Sentiment_Data_v1.0.csv', encoding="utf8") as f:
        for line in f:
          
          if firstline:
              firstline = False
          else:
            
            parts = line.split(',')
            neg = 0
            pos = 0
            # The emojis with Total ocurrences < 5 are excluded
            if int(parts[2]) > 4:
              # Negative score = Negative ocurrences / Total Ocurrences
              neg = float(parts[4]) / float(parts[2])
              # Positive score = Positive ocurrences / Total Ocurrences
              pos = float(parts[6]) / float(parts[2])
              # Final score = Positive score - Negative score
              score = pos - neg
              # print(score)
              # position = unicode(parts[0], "utf-8")
              position = parts[0].encode("utf-8")
              emojis[position] = score
    return emojis
    
def clean_emojis(emojis):
    """
    Remove emojis with |score| < 0.25
    """
    new_emojis = {}
    for emoji, score in emojis.items():
        if score < -0.25:
            new_emojis[emoji] = score
        elif score > 0.25:
            new_emojis[emoji] = score
    return new_emojis

def read_words():
    """
    Reads the classified words from ML-SentiCon paper.
    Remove words with |score| < 0.50
    """
    # root = ET.parse('./catalan-sentiment-analysis-master/src/data/senticon.ca.xml').getroot()
    root = ET.parse('./Datos/ML-SentiCon/senticon.es.xml').getroot()
    words = {}
    for word in root.iter('lemma'):
        if not isinstance(word.text, str):
            uni = unicode(word.text.replace(' ', '').replace('_', ' '), "utf-8")
        else:
            uni = word.text.replace(' ', '').replace('_', ' ')
        score = float(word.attrib['pol'])
        if score > 0.2:
            words[uni] = score
        elif score < -0.2:
            words[uni] = score
    return words
    
# EMOJIS
print('Reading emojis...')
emojis = read_emojis()
print('Cleaning emojis...')
emojis = pd.DataFrame.from_dict(clean_emojis(emojis), orient='index')
print('Emoji dict len: {}'.format(len(emojis)))

# WORDS
print('Reading words...')
words = pd.DataFrame.from_dict(read_words(), orient='index')
print ('Words dict len: {}'.format(len(words)))

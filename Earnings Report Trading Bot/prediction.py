#FUNCTIONS
from nltk.sentiment.vader import SentimentIntensityAnalyzer
import re
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import mean_squared_error
from nltk import sent_tokenize
from nltk.corpus import stopwords
import nltk
nltk.download('stopwords')


# DEFINE PATH TO THE "tester" FOLDER
path = '/Users/len24/Desktop/BUS306C/earnings_report/tester/'

def sentiment(text):
    # return SentimentIntensityAnalyzer().polarity_scores(text)['compound']

    # Tokenize the text into sentences
    sentences = sentence_tokenize(text)
    # Analyze the sentiment of each sentence
    sentence_scores = [SentimentIntensityAnalyzer().polarity_scores(sentence)['compound'] for sentence in sentences]
    # Calculate the average sentiment score across sentences
    avg_sentiment = sum(sentence_scores) / len(sentence_scores) if len(sentence_scores) > 0 else 0
    return round(avg_sentiment, 2)

def sentence_sentiments(text):
    sentences = sent_tokenize(text)
    return [sentiment(sentence) for sentence in sentences]


def syl(word):
    word = word.lower()
    count = 0
    vowels = 'aeiouy'
    if word[0] in vowels:
        count += 1
    for i in range(1, len(word)):
        if word[i] in vowels and word[i-1] not in vowels:
            count += 1
        if word[-1] == 'e':
            count -= 1
        if count == 0:
            count += 1
    return count

def word_tokenize(text):
    text = text.lower()
    text=re.sub('[^\w\s]','',text)
    return text.split()

def advanced_tokenizer(text):
    stop_words = set(stopwords.words('english'))
    tokens = word_tokenize(text)
    return [token for token in tokens if token.isalnum() and token not in stop_words]

def sentence_tokenize(text):
    from nltk import sent_tokenize
    return sent_tokenize(text)

def fog(text):
    t_list = word_tokenize(text)
    word_count = len(t_list)
    complex_count = 0
    for word in t_list:
        if syl(word) >= 3:
            complex_count += 1
    sentences = len(sentence_tokenize(text))
    fog_index = 0.4 * ((word_count/sentences) + (100 * (complex_count/word_count)))
    return round(fog_index, 2)

# PASTE IN YOUR FUNCTION
def predict_return():
    # Initialize the score
    score = 0

    # Calculate sentiment score for the entire document
    doc_sentiment = sentiment(mda)

    # Add sentiment scores for each sentence
    sentence_sentiments_list = sentence_sentiments(mda)

    # Use a more sophisticated tokenizer
    tokens = advanced_tokenizer(mda)

    # Feature Engineering: Combine features
    features = [
        doc_sentiment,
        fog(mda),
        p5d_market_perf,
        p5d_firm_perf,
        py_size,
        py_pe_ratio,
        len(tokens),
        sum(sentence_sentiments_list),
        max(sentence_sentiments_list),
    ]

    # You may need to fine-tune the coefficients based on your specific dataset and problem

    # Weighted sum of features (you may adjust the coefficients)
    score += 0.1 * features[0]  # sentiment score
    score -= 0.5 * features[1]  # fog index
    score += 0.1 * features[2]  # prior 5-day market performance
    score += 0.5 * features[3]  # prior 5-day firm performance
    # score += 0.1 * features[4]  # prior year market cap
    score += 0.1 * features[5]  # prior year EPS
    score -= 0.0 * features[6]  # number of tokens
    score += 0.2 * features[7]  # sum of sentence sentiment scores
    score += 0.1 * features[8]  # max sentence sentiment score

    # Scale the score (if needed)
    score /= 100

    # Keep the score within the appropriate range
    # score = max(-1, min(1, score))

    return score


sample=['2018 aapl','2019 aapl','2020 aapl','2021 aapl','2022 aapl',\
'2018 wmt','2019 wmt','2020 wmt','2021 wmt','2022 wmt',\
'2018 nflx','2019 nflx','2020 nflx','2021 nflx','2022 nflx',\
'2018 cvx','2019 cvx','2020 cvx','2021 cvx','2022 cvx',\
'2018 nke','2019 nke','2020 nke','2021 nke','2022 nke',\
'2018 abbv','2019 abbv','2020 abbv','2021 abbv','2022 abbv']
sample.sort()

def test():
  result=0
  global mda
  global py_size
  global py_pe_ratio
  global p5d_market_perf
  global p5d_firm_perf
  for firm_year in sample:
    f=open(path+firm_year+'.txt','r',encoding='utf-8')
    data=f.read().split('\n!\n')
    py_size=float(data[0])
    py_pe_ratio=float(data[1])
    p5d_market_perf=float(data[2])
    p5d_firm_perf=float(data[3])
    mda=data[5].lower()
    actual_return=float(data[4])
    predict=predict_return()
    print (firm_year)
    print ('prediction:')
    print (predict)
    print ('actual return')
    print (actual_return)
    if predict>0 and actual_return>0:
      roi=(100000*predict*actual_return)
    elif predict<0 and actual_return<0:
      roi=(100000*predict*actual_return)
    elif predict<0 and actual_return>0:
      roi=(100000*predict*actual_return)
    elif predict>0 and actual_return<0:
      roi=(100000*predict*actual_return)
    print('ROI with $100,000 available:')
    print(round(roi,2))
    print('')
    result+=roi
  print('aggregate results:')
  print(round(result,2))

test()
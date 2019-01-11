# senseasim

linux users use aliases, e.g.:

` alias sensevectors='Rscript --vanilla --default-packages=methods,utils,stats bin/sensevectors' `

create an R environment file which contains the following content:

``
DATA_HOME='~/data'
DATA_TEMP='~/data/temp'
WNHOME='~/data/wordnet/WordNet-3.0'
MAILGUN_API='https://api:key-xyz@api.mailgun.net/v3/zyx.mailgun.org/messages'
MAILGUN_RECEPIENT='name@server.com'
RETICULATE_PYTHON='/usr/local/miniconda3/bin/python3'
``

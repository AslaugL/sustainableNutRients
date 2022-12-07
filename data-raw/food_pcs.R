library(dplyr)

#Ingredients that are counted as individual pieces if found in recipes without a unit
pcs = c('anchovy', 'anchovy fillet', 'anise', 'apple', 'apricot', 'avocado', 'artichoke', 'aubergine',

        'banana', 'bay leaf', 'bean', 'baguette', 'bok choi', 'bouquet garni', 'broccoli', 'broth cube', 'basil',
        'bread',

        'cabbage', 'cardamom', 'carrot', 'cauliflower', 'Celariac root', 'celery', 'celery stalk',
        'champignons', 'chicken', 'chicory', 'chili', 'ciabatta', 'cinnamon', 'clementine', '\\bcloves\\b', 'cod',
        'cod fillet', 'crab', 'cracker cream', 'cucumber', 'coriander',

        'duck',

        'egg', 'entrecôtekam', 'eggplant',

        'fennel', 'fig',

        'garlic', 'grape',

        'hamburger bun', 'hen breast fillet grouse', 'herring smoked',

        'jerusalem artichoke', 'juniper berry',

        'kiwi',

        'lamb chop', 'leek', 'lemon', 'lemongrass', 'lettuce', 'lime',

        'mango', 'marshmallow', 'mushroom',

        'nori seaweed', 'nut',

        'olive', 'onion', 'orange',

        'paprika', 'pear', 'pepper', 'peppercorns', 'pineapple', 'pomegranate', 'plate', 'pork',
        'pork sausage', 'potato', 'prawn', 'prune',

        'radish', 'roll',

        'salad', 'salmon', 'sausage', 'scallion', 'scallop', 'scampi', 'shallot', 'sheep', 'sheet', 'shrimp',
        'squid', 'stock cube',

        'tenderloin', 'thyme', 'tomato', 'tortilla', 'trout', 'turkey', 'turnip',

        'zucchini')

#Save
saveRDS(pcs, "./data-raw/pcs.Rds")

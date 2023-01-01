composite_ingredients_oda <- tibble(

  #Find recipes of the composite ingredients
  tmp = c(
    #From Oda webstore
    "raw vegetables_assorted;34 g carrot
    33 g parsnip
    33 g swede",
    #From https://cooking.nytimes.com/recipes/1022154-vegetarian-bolognese
    "bolognese_vegetarian;3 tablespoons unsalted butter
    2 tablespoons extra-virgin olive oil
    1 large yellow onion, roughly chopped into ¼-inch pieces
    2 medium carrots, roughly chopped into ¼-inch pieces
    1.25 pound cauliflower
    0.75 cup tomato paste
    2 tablespoons low-sodium soy sauce
    4 garlic cloves, coarsely chopped
    0.75 cup whole milk
    1 fresh bay leaf or thyme sprig
    1 pound rigatoni or another ridged dried pasta, or fresh pappardelle or tagliatelle
    0.5 cup finely grated Parmesan, plus more for serving",
    #Lapskausblanding, oda
    "vegetable mix_frozen;63 g potato
    20 g carrot
    9 g celeriac
    8 g leek",
    #Inspiredtaste.net hummus
    "hummus;250 g chickpeas
    60 ml lemon juice
    60 ml tahini
    1 clove garlic
    30 ml olive oil
    0.5 tsp cumin
    2.5 tbsp water",
    #Inspredtaste.net coleslaw
    "coleslaw;2 pound cabbage
    2 carrots
    0.5 cup fresh parsley
    170 g mayonnaise
    2 tbsp apple cider vinegar
    2 tbsp mustard
    0.25 tsp salt
    1.5 tsp sugar",
    #Cookie and Kate tzatziki
    "tzatziki;10 ounce cucumber
    1.5 cup plain greek yoghurt
    2 tbsp olive oil
    2 tbsp fresh dill
    1 tbsp lemon juice
    1 clove garlic
    0.5 tsp sea salt",
    #Minimalist baker chana masala
    "chana_masala;3 tbsp coconut oil
    1 onion
    1 tbsp ground cumin
    0.75 tsp salt
    6 clove garlic
    2 tbsp fresh ginger
    0.5 cup cilantro
    2.5 pcs green chili
    1 tbsp ground coriander
    1 tsp chili powder
    1 tsp ground turmeric
    28 ounce tomatoes
    15 ounce canned chickpeas
    1 tsp garam masala
    2.5 tsp coconut sugar
    2 tbsp lemon juice",
    #Simply Recipe's lasagna
    "lasagna;2 tsp olive oil
    1 pound beef chuck
    0.5 pcs onion
    0.5 pcs bell pepper
    2 clove garlic
    28 ounce tomato sauce
    3 ounce tomato paste
    14 ounce tomatoes
    2 tsp dried oregano
    0.25 cup parsley
    1 tbsp italian seasoning
    1 pinch garlic powder
    1 tbsp red wine vinegar
    2 tbsp sugar
    0.5 pound pasta
    15 ounce ricotta
    11.5 pound mozzarella cheese
    0.25 pound parmesan cheese",
    #Oat cream recipe
    "cream_plant-based;2 dl oat milk
    1 dl rapeseed oil",
    #The edgy veg Vegan minced meat
    "plant based_minced meat;1 cup walnuts
    16 oz mushrooms
    3 tbsp olive oil
    2 tbsp soy sauce
    1 tsp ground cumin
    2 tsp ground coriander
    0.5 tsp salt
    1 tsp white pepper
    2 tsp smoked paprika powder
    2 tsp garlic powder
    1.5 tsp onion powder
    0.25 tsp chili powder",
    #Oda smoothie mix
    "smoothie mix_tropical;35 g pineapple
    35 g mango
    30 g banana",
    #The mediterraneandish falafels
    "falafel;2 cup dried chickpeas
    0.5 tsp baking soda
    1 cup fresh parsley
    0.75 cup fresh cilantro
    0.5 cup fresh dill
    1 onion
    7.5 clove garlic
    1 tbsp ground black pepper
    1 tbsp ground cumin
    1 tsp cayenne pepper
    1 tsp baking powder
    2 tbsp sesame seeds",
    #Thai soup for shrimp
    "soup_thai;2 tbsp butter
    2 clove garlic
    1 onion
    1 red bell pepper
    1 tbsp fresh ginger
    2 tbsp curry paste
    12 ounce coconut milk
    3 cup chicken stock
    1 lime the juice of
    2 tbsp fresh cilantro",
    #tine porridge
    "porridge;2 dl rice
    4 dl water
    10 dl whole milk
    1 ts salt
    2 tbsp butter
    1 dl sugar
    2 tbsp cinnamon",
    #Baker betty cookies
    "cookie_cookie dough;113 g butter
    150 g brown sugar
    1 egg
    1 tsp vanilla extract
    0.5 tsp baking powder
    0.5 tsp salt
    150 g all purpose flour
    150 g semi-sweet chocolate chips",
    #The garden grazer mexican lentil bean salad
    "bean salad_mexican lentil;1 cup green lentils dry
    15 ounce can black beans
    1 red bell pepper
    0.33 cup onion
    6 oz cherry tomatoes
    0.67 cup cilantro
    2 tbsp lime juice
    1 tsp dijon mustard
    1.5 clove garlic
    0.5 tsp ground cumin
    0.5 tsp dried oregano
    0.25 tsp salt",
    #The recipe well bean and olive salad
    "bean salad;19 oz cannellini beans canned
    19 oz kidney beans canned
    15 oz can kidney beans
    0.5 pcs cucumber
    2 cup cherry tomatoes
    1 bell pepper
    0.5 cup black olives
    0.5 cup fresh parsley
    0.5 cup red onion
    2 tbsp fresh basil
    0.75 cup feta cheese",
    #Joyfood sunshine chocolate chip cookies
    "biscuit_chocolate chip;1 cup butter
    1 cup white sugar
    1 cup brown sugar
    2 tsp vanilla extract
    2 eggs
    3 cup all purpose flour
    1 tsp baking soda
    1 tsp salt
    12 oz chocolate chips",
    #Sugar salt magic homemade oreos
    "biscuit_oreo;130 g flour
    35 g corn starch
    0.33 cup cocoa
    0.5 tsp baking soda
    95 g unsalted butter
    1 tbsp milk
    113 g unsalted butter
    1.5 cup confectioners sugar
    1 tsp vanilla extract",
    #Tasted recipes homemade salted biscuits
    "biscuit_salt; 60 g butter
    50 g sugar
    1 tbs sesame seed
    1 tsp baking soda
    136 g all purpose flour
    1 tsp salt
    4.5 tbsp milk",
    #Damn delicious sourdough biscuits, sourdough starter from the clever carrpt
    "biscuit_sourdough;2 cup all purpose flour
    2 tsp sugar
    2 tsp baking powder
    1 tsp salt
    0.75 tsp baking soda
    0.5 cup unsalted butter
    0.5 cup buttermilk
    60 g whole wheat flour
    60 g water",
    #Becky's mindful kitchen Spelt biscuits
    "biscuit_spelt;2 cup spelt flour
    1.5 tsp salt
    1 tsp baking powder
    0.25 cup olive oil
    0.85 cup water",
    #Dinner then desert sweet and sour chicken
    "chicken_sweet and sour;1.5 pound chicken breast
    0.5 cup cornstarch
    2 eggs
    0.25 cup flour
    1 cup pineapple
    1 red bell pepper
    1 green bell pepper
    0.5 pcs onion
    0.5 cup sugar
    0.25 cup brown sugar
    0.33 cup ketchup
    4 tsp soy sauce
    2 clove garlic",
    #Cookie and cate chili sin carne
    "chili_sin carne;2 tbsp olive oil
    1 onion
    1 red bell pepper
    2 carrots
    2 stalk celery
    0.5 tsp salt
    4 clove garlic
    2 tbsp chili powder
    2 tsp cumin
    1.5 tsp smoked paprika powder
    1 tsp dried oregano
    28 ounce canned tomatoes
    30 ounce canned black beans
    15 ounce canned pinto beans
    2 cups water
    1 bay leaf
    2 tbsp fresh cilantro
    1.5 tsp lime juice",
    #Homemade home chocolate mousse miz
    "chocolate_mousse powder;1.25 cup sugar
    1 cup cornstarch
    1 cup milk powder
    0.25 cup cocoa powder
    0.5 pcs vanilla bean",
    #Chocolate covered cate chocolate spread
    "chocolate_spread; 240 g hazelnuts
    1.5 tbsp vanilla extract
    0.25 cup cocoa powder
    0.33 cup honey
    0.25 tsp salt
    0.5 cup milk
    2 tbsp coconut oil",
    #31 daily oat crackers
    "cracker_oat;0.5 cup walnuts
    1.75 cup rolled oats
    1 tsp salt
    1 tsp baking powder
    2.25 tsp italian seasoning
    0.25 tsp garlic powder
    1 cup milk",
    #Meny fish balls
    "fish_ball;600 g cod
    0.5 tsp salt
    1 tbsp potato starch
    1 egg
    4 dl whole milk",
    #BBC good food Fish cakes
    "fish_cake;450 g cod;
    2 leaf bay leaf
    150 ml milk
    350 g potatoes
    0.5 tsp lemon zest
    1 tbsp chpped parsley
    1 tbsp chives
    1 egg
    85 g breadcrumbs
    3.5 tbsp sunflower oil",
    #Crnuchy creamy sweet fish sticks
    "fish_stick;2 fillet cod
    0.5 tsp salt
    0.25 tsp black pepper
    1 egg
    2 tbsp cream
    0.25 cup all purpose flour
    0.33 cup breadcrumbs
    0.25 tså garlic powder
    0.25 tsp italian seasoning
    0.5 cup vegetable oil",
    #Ricardo cuisine meatballs in tomato sauce
    "meatball_tomato sauce;3 clove garlic
    1 leaf bay
    0.25 tsp red pepper flakes
    30 ml olive oil
    28 oz tomatoes
    2 slice white bread
    60 ml milk
    450 g ground beef
    150 g parmesan cheese
    1 egg
    1 clove garlic
    10 g chopped parsley
    0.5 tsp salt
    0.5 tsp ground fennel
    0.25 tsp dried oregano
    0.25 tsp red pepper flakes",
    #BBC good food ravioli
    "pasta_filled;300 g all purpose flour
    2 eggs
    2 egg yolks
    1 tbsp olive oil
    2 clove garlic
    200 g baby spinach
    0.5 lemon zested
    250 g ricotta cheese",
    #Oda curry paste
    "curry_paste;3 tbsp coriander seed
    1 tsp fennel seed
    1 tsp pepper
    1 tsp tumeric
    2 tbsp cumin
    1 tsp cinnamon
    1 tsp paprika powder
    1 tsp salt
    1 pcs chili pepper
    4 clove garlic
    1 tsp fresh ginger
    4 tbsb white wine vinegar
    1 tbsp tomato puree",
    #Matprat langpanne Pizza base
    "pizza_base;3 dl water
    0.5 pack yeast
    2 tbsp oil
    0.5 tsp salt
    7 dl all purpose flour",
    #Also use as pizza dough
    "pizza_dough;3 dl water
    0.5 pack yeast
    2 tbsp oil
    0.5 tsp salt
    7 dl all purpose flour",
    #Matprat crem potatoes
    "potato_cream;4 dl cream
    1 clove garlic
    0.5 tsp salt
    0.25 tsp pepper
    1 kg potato
    1 tbsp butter
    1 dl grated cheese",
    #Matprat potato mash
    "potato_mash;10 potatoes
    2 dl milk
    2 tbsp butter
    0.5 tsp salt
    0.5 tsp pepper
    0.25 tsp nutmeg",
    #Klikk salt sticks
    "salt_stick; 4 dl wheat flour
    1.5 dl water
    10 g yeast dry
    0.5 tsp salt",
    #BBC goodfood vegetable spread
    "vegetable_spread;390 g chickpeas
    1 clove garlic
    4 tbsp olive oil
    1 tsp lemon juice
    2 red bell peppers",
    #Happy veggie chicken bean taco
    "bean_taco;2 sweet peppers
    2 onions
    400 g canned black beans
    5 tbsp sweet chili sauce
    2 tbsp soy sauce
    2 tsp smoked paprika powder
    0.5 tsp chili powder
    1 tsp ground cumin
    0.5 ground coriander",
    #BBC good foods chicken satay
    "chicken_satay;1 cm ginger
    2 clove garlic
    1 lime
    1 tsp honey
    1 tbsp soy sauce
    1 tbsp curry powder
    3 tbsp peanut butter
    500 g skinless chicken breast
    165 ml coconut milk
    1 tsp vegetable oil",
    #Matprat pølsegryte/sausage stew
    "sausage_stew;400 g sausage
    0.5 pcs broccoli
    1 pcs onion
    1 clove garlic
    400 g whole wheat pasta
    800 g canned tomatoes
    4 dl water
    1 ts dried oregano
    100 g grated cheese
    2 dl cream
    1 tsp sugar
    100 g frozen peas"
  )) %>%
  #Format
  separate(., tmp, into = c("recipe_name", "Ingredients"), sep = ";") %>%
  separate_rows(., Ingredients, sep = "\\n")

#Save
saveRDS(composite_ingredients_oda, "./data-raw/composite_ingredients_oda.Rds")

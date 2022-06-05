# Chez Grater

![chez-grater](https://github.com/dfithian/chez-grater/actions/workflows/workflow.yml/badge.svg)

Scrape and parse recipe blogs to skip the ads. Brains behind https://app.mo-nomz.com/.

Feedback and enhancements via PR welcome.

## Usage

Check [./app/main.hs](./app/main.hs) for an example.

```bash
$ cabal v2-run chez-grater -- https://www.halfbakedharvest.com/southern-butter-biscuits
```

```text
Flaky Southern Butter Biscuits with Strawberry Bourbon Jam - Half Baked Harvest
-------------------------------------------------------------------------------
3 1/2 cup all-purpose flour
2 1/2 tsp baking powder
1/4 tsp kosher salt
2 sticks (16 tablespoons) cold salted butter, cubed, plus melted butter, for brushing
1 cup cold buttermilk
2 oz cold cream cheese, sliced into 3 slices (optional)
flaky sea salt
3 1/2 cup fresh or frozen strawberries, chopped
1/4 cup honey
2 tsp lemon juice
2 tbsp strawberry jam
1 tbsp bourbon
-------------------------------------------------------------------------------
- Preheat the oven to 400° F.
- In a food processor, combine the flour, baking powder, salt, and butter. Pulse until the mix clumps together to form larger pea-size balls. Drizzle in the buttermilk until the dough is "shaggy" looking. The dough will be a little dry.
- Turn the dough out onto a clean surface and pat into a 1-inch thick square. Cut the dough into 4 pieces. Add 1 slice of cream cheese to 3 of the squares, gently pressing to flatten. Stack the cream cheese topped pieces on top of each other. Add the final piece (with no cream cheese) on top of the stack (see above photos). Press down to flatten. Dust the surface with flour and roll the dough into a 1 inch thick rectangle. Cut into 12 biscuits. Transfer to a parchment-lined baking sheet and freeze 10-15 minutes, to chill.
- Remove the biscuits from the freezer and brush with melted butter. Transfer to the oven and bake for 20-25 minutes, until golden on top.
- Meanwhile, make the jam. Add the strawberries, honey, and lemon juice to a medium size pot over high heat. Bring to a boil, to cook for 5-8 minutes or until the jam has reduced and thickened by 1/3. Stir in the jam and bourbon, cook 1-2 minutes, remove from the heat.
- Serve the biscuits warm with butter and jam. I like sea salt on top too!
```

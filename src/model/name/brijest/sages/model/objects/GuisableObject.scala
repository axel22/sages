package name.brijest.sages.model.objects


import name.brijest.sages.model.quantities.IntNum
import name.brijest.sages.model._


abstract class GuisableObject extends Object {
  define("Guise", "Appearance index.", IntNum(0))
  define("Total guises", "Total possible appearances.", IntNum(1))
}





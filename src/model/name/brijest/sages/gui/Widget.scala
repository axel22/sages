package name.brijest.sages.gui


import name.brijest.sages.controller.Controller


trait Widget {
  var controller: Controller = null
  def width: Int
  def height: Int
}

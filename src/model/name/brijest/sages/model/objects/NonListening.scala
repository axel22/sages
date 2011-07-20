package name.brijest.sages.model.objects


import name.brijest.sages.model._



trait NonListening {
  def events = Nil
  def onEvent(e: Event) {}
}

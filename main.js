cc.game.onStart=function() {
  let f=cc.game["onStartMain"];
  return f.apply(this, Array.prototype.slice.call(arguments,0));
}
cc.game.run();
console.log("cc.game.run() called");


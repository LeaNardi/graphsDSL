{
  emptygraph g;

  let i = 1;
  let j = 2;

  repeat {
    set j = i + 1;
    repeat {
      addedge g i j (i * j);
      set j = j + 1
    } until not (j < 6);
    set i = i + 1
  } until not (i < 5);

  kruskal g
}

import { Tree, Forest, Name } from "../src";
import * as L from "@lieene/ts-utility";

test("poly tree test", () =>
{
    let t = Tree(Name("I'am the root"));
    t.push(Name('1st node')).add(Name('c00')).add(Name('c01')).add(Name('c02'));
    t.push(Name('2nd node'));
    t.add(Name('3rd node'));
    t.add(Name('4th node'));
    t.root.child(1)!.push(Name('C10')).push(Name('C100'));
    t.root.child(2)!.add(Name('C20')).add(Name('C21'));
    console.log(t.info(true));
    let t2 = t.clone(true);
    console.log(t2.info(true));
    let t3 = t.clone(true, n => n.depth <= 1);//.polymorph(n=>named("clone tree"));
    console.log(t3.info(true));
    let t4 = t.clone(true, n => n.depth <= 1, n => n.poly(Name('clone'))).polymorph(Name("clone tree"));
    console.log(t4.info(true));
    t4.merg(t3, 0);
    t4.merg(t3, 1, false);
    t4.merg(t2);
    //t4.merg(t3,0,false);
    console.log(t4.info(true));
    t4.root.child(1)!.remove();
    t4.root.child(5, 1)!.remove();
    console.log(t4.info(true));
    console.log(t3.info(true));
    console.log(t2.info(true));
    console.log(t.info(true));
    console.log(t.root.child(1)!.subTreeInfo());

    let t4s = Tree.Simplify(t4);
    let f = Forest<Name, Name, Name>(Name('the forest'));
    f.trees.push(t2 as any, t3 as any, t4 as any);

    //let a = Tree().root.morph({ a: "s", b: 1 }).tree.morph({source:"sss"});
    let a = Tree().polymorph<{ a: number }, { source: string }[]>();
    //let a = Tree();
    let b = Tree.Simplify(a);
    //let c = b.polymorph<{ a: number }, { source: string }>();
    let dd: Tree.MorphNode<{ a: number }, { b: string }> = L.Any;
    let e = Tree.Nomalize(a);
    let g = Tree.Edit(b);
    g.setLogger(()=>"logloglog");
    console.log(g.info(false));
});
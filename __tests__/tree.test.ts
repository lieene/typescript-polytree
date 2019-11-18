import { Tree as tr, Forest as ft, Named as nt } from "../src";
import * as L from "@lieene/ts-utility";

test("poly tree test", () =>
{
    let t = tr(nt.CreateName("I'am the root"));
    t.push(nt.CreateName('1st node')).add(nt.CreateName('c00')).add(nt.CreateName('c01')).add(nt.CreateName('c02'));
    t.push(nt.CreateName('2nd node'));
    t.add(nt.CreateName('3rd node'));
    t.add(nt.CreateName('4th node'));
    t.root.child(1)!.push(nt.CreateName('C10')).push(nt.CreateName('C100'));
    t.root.child(2)!.add(nt.CreateName('C20')).add(nt.CreateName('C21'));
    console.log(t.info(true));
    let t2 = t.clone(true);
    console.log(t2.info(true));
    let t3 = t.clone(true, n => n.depth <= 1);//.polymorph(n=>named("clone tree"));
    console.log(t3.info(true));
    let t4 = t.clone(true, n => n.depth <= 1, (n, o) => n.poly(nt.CreateName('clone'))).polymorph(nt.CreateName("clone tree"));
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

    let t4s = tr.Simplify(t4);
    let f = ft<nt.Name>(nt.CreateName('the forest'));
    f.trees.push(t2, t3, t4);



    //let a = tr().root.morph({ a: "s", b: 1 }).tree.morph({source:"sss"});
    let a = tr().polymorph<{ a: number }, { source: string }[]>();
    //let a = tr();
    let b = tr.Simplify(a);
    //let c = b.polymorph<{ a: number }, { source: string }>();
    let dd: tr.MorphNode<{ a: number }, { b: string }> = L.Any;
    let e = tr.Nomalize(a);
    let g = tr.Edit(b);
});
import { Tree, Forest, Name } from "../src";
import * as L from "@lieene/ts-utility";

test("poly tree test", () =>
{
    let t1 = Tree<Name, Name>("Simple", n => Name("the root"), Name("I'am the tree"));
    Tree.Edit(t1).push(Name('1st node')).add(Name('c00')).add(Name('c01')).add(Name('c02'));
    Tree.Edit(t1).push(Name('2nd node'));
    Tree.Edit(t1).add(Name('3rd node'));
    Tree.Edit(t1).add(Name('4th node'));
    Tree.Edit(t1).root.child(1)!.push(Name('C10')).push(Name('C100'));
    Tree.Edit(t1).root.child(2)!.add(Name('C20')).add(Name('C21'));
    let nn1 = t1.findByName<typeof t1>("the root");
    console.log(nn1.join("-"));
    console.log(Tree.Nomalize(t1).info(true));
    let t2 = Tree.Edit(t1.clone(false));
    console.log(t2.info(true));
    let t3 = Tree.Nomalize(t1).clone<Name, Name>(true, n => n.depth <= 1);//.polymorph(n=>named("clone tree"));
    console.log(t3.info(true));
    let t4 = Tree.Nomalize(t1).clone<Name, Name>(true, n => n.depth <= 1, n => n.poly(Name('clone')));
    t4.poly(Name("clone tree"));
    console.log(t4.info(true));
    t4.merg(t3, 0);
    t4.merg(t3.root, 0);
    t4.merg(t3, 1, false);
    t4.root.merg(t2);
    //t4.merg(t3,0,false);
    console.log(t4.info(true));
    t4.root.child(1)!.remove();
    t4.root.child(6, 1)!.remove();
    console.log(t4.info(true));
    console.log(t3.info(true));
    console.log(t2.info(true));
    console.log(Tree.Nomalize(t1).info(true));
    console.log(Tree.Nomalize(t1.root.child(1)!).subTreeInfo());
    let tm = Tree((n) => ({ b: 2 }), Name('tm'), { a: 1 });
    let tx = Tree((n) => ({ b: 2 }), Name('tx'), { a: 1 });
    tx.merg(tm, false);
    tm.merg(tx, false);
    
    let nnt = tm.findByName<typeof tm>(undefined);
    console.log(nnt.join("-"));
    
    let t4s = Tree.Simplify(t4);
    let f = Forest<Name, Name, Name>(Name('the forest'));
    f.trees.push(Tree.Edit(t1), t2, t3, t4);
    let ttt = f.trees[0];
    let nn = ttt.nodes[0];
    let nnn = f.findByName("c01");
    console.log(nnn.join("-"));

    //let a = Tree().root.morph({ a: "s", b: 1 }).tree.morph({source:"sss"});
    let a = Tree().polymorph<{ a: number }, { source: string }[]>();
    //let a = Tree();
    let b = Tree.Simplify(a);
    //let c = b.polymorph<{ a: number }, { source: string }>();
    let dd: Tree.MorphNode<{ a: number }, { b: string }> = L.Any;
    let e = Tree.Nomalize(a);
    let g = Tree.Edit(b);
    g.setLogger(() => "logloglog");
    console.log(g.info(false));
    let nt=Name.NamedTree('root','tree');
    console.log(nt.info(true));
});
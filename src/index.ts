// File: index.ts                                                                  //
// Project: lieene.poly-tree                                                       //
// Created Date: Mon Nov 11 2019                                                   //
// Author: Lieene Guo                                                              //
// MIT License                                                                     //
// Copyright (c) 2019 Lieene@ShadeRealm                                            //

import '@lieene/ts-utility';
import * as L from '@lieene/ts-utility';

namespace core
{
  export class CNode
  {
    // implements INode<CNode, CRoot>//, INodeEdit<CNode, CRoot>
    constructor(tree: CTree | undefined, index: number, peerIndex: number, parentID: number | undefined)
    {
      this.tree = tree!;
      this.index = index;
      this.peerIndex = peerIndex;
      this.parentID = parentID;
      this.nodeInfo = func.nodeInfo.bind(this);
    }
    toString = func.detailInfo;
    tree: CTree;
    //#region INodeIdx --------------------------------------------------------------------------
    index: number;
    peerIndex: number;
    parentID: number | undefined;
    childrenID: number[] = [];
    get subTreeRange(): L.Range
    { return new L.Range([this.index, func.subTreeEnd.call(this)]); }
    //#endregion --------------------------------------------------------------------------------

    //#region INodeAccess -----------------------------------------------------------------------
    //#region INodeSimple -----------------------------------------------------------------------
    get parent(): CNode | undefined { return this.parentID === undefined ? undefined : this.tree.nodes[this.parentID]; }
    get children(): CNode[] { return this.childrenID.map(id => this.tree.nodes[id]); }
    get deepChildren(): CNode[] { let range = this.subTreeRange; return this.tree.nodes.slice(range.start + 1, range.end); }
    get childCount(): number { return this.childrenID.length; }
    child = func.child;
    //#endregion --------------------------------------------------------------------------------
    get isRoot(): boolean { return this.parentID === undefined; }
    get isBranch(): boolean { return this.childrenID.length > 0; }
    get isLeaf(): boolean { return this.childrenID.length === 0; }
    get depth(): number { let depth = 0, p = this.parent; while (p !== undefined) { depth++; p = p.parent; } return depth; }
    isChildOf: (node: CNode) => boolean = func.isChildOf;
    findChild = func.findChild;
    forAcending = func.forAcending;
    forDecending = func.forDecending;
    subTreeInfo = func.treeInfo;
    nodeInfo: (detail?: boolean) => string;
    setLogger = func.OverrideExtraLog;
    //#endregion --------------------------------------------------------------------------------

    //#region INodeEdit -------------------------------------------------------------------------
    //#region INodeEdit -------------------------------------------------------------------------
    push = func.push;
    add = func.add;
    clone = func.clone;
    merg = func.merg;
    remove = func.remove;
    //#endregion --------------------------------------------------------------------------------

    //#region INodeMorphing ---------------------------------------------------------------------
    poly = func.poly;
    morph = func.morph;
    polySub = func.polySub;
    morphSub = func.morphSub;
    //#endregion --------------------------------------------------------------------------------
  }

  export class CTree
  {
    // extends CNode implements IRoot<CNode, CRoot>
    constructor(empty: boolean = false)
    {
      if (!empty) { this.nodes.push(new CNode(this, 0, 0, undefined)); }
      this.info = func.treeInfo.bind(this);
    }

    nodes: CNode[] = [];
    toString(): string { return this.info(true); }

    //#region ITreeAccess -----------------------------------------------------------------------
    //#region ITreeSimple -----------------------------------------------------------------------
    get root(): CNode { return this.nodes[0]; }
    get tail(): CNode { return this.nodes.last!; }
    get nodeCount(): number { return this.nodes.length; }
    //#endregion --------------------------------------------------------------------------------
    findNode = func.findNode;
    info: (detail?: boolean) => string;
    setLogger = func.OverrideExtraLog;
    //#endregion --------------------------------------------------------------------------------

    //#region ITreeEdit -------------------------------------------------------------------------
    push = func.push;
    add = func.add;
    clone = func.clone;
    merg = func.merg;
    //#endregion --------------------------------------------------------------------------------

    //#region ITreeMorphing ---------------------------------------------------------------------
    poly = func.poly;
    morph = func.morph;
    polymorph = func.polymorph;
    //#endregion --------------------------------------------------------------------------------
  }

  export class CForest
  {
    trees: Array<CTree> = [];
    polymorph<N extends object>():
      Typing.IForest<Typing.MorphTreeN<N>>;
    polymorph<N extends object, T extends object>():
      Typing.IForest<Typing.MorphTree<N, T>>;
    polymorph<N extends object, T extends object, F extends object>():
      L.Extend<Typing.IForest<Typing.MorphTree<N, T>>, F>;
    polymorph<F extends object>(forestExt: F):
      L.Extend<Typing.IForest<Typing.Tree>, F>;
    polymorph<F extends object[]>(...forestExt: F):
      L.Extend<Typing.IForest<Typing.Tree>, L.MergTupleType<F>>;
    polymorph<N extends object, T extends object, F extends object>(nodeExt: (node: CNode) => N, treeExt: (node: CTree) => T, forestExt: F):
      L.Extend<Typing.IForest<Typing.MorphTree<N, T>>, F>;
    polymorph<T extends object, F extends object[]>(treeExt: (node: CTree) => T, ...forestExt: F):
      L.Extend<Typing.IForest<Typing.MorphTreeT<T>>, L.MergTupleType<F>>;
    polymorph<N extends object, T extends object, F extends object[]>(nodeExt: (node: CNode) => N, treeExt: (node: CTree) => T, ...forestExt: F):
      L.Extend<Typing.IForest<Typing.MorphTree<N, T>>, L.MergTupleType<F>>;
    polymorph(...ext: any[]): any
    {
      let [first, second, ...rest] = ext;
      if (L.IsFunction(first))
      {
        if (L.IsFunction(second))
        {
          this.trees.forEach(t => { t.polymorph(first, second(t) as object); });
          ext = rest;
        }
        else
        {
          this.trees.forEach(t => t.polymorph(first));
          ext = [second, rest];
        }
      }
      ext.forEach(x => L.assign(this, x, L.AssignFilter.extend));
      return this;
    }

    toString(): string { return this.trees.join('\n'); }
  }
}

namespace func
{
  import MorphNodeNX = Typing.MorphNodeNX;
  import MorphTreeX = Typing.MorphTreeX;
  import MorphTreeNX = Typing.MorphTreeNX;
  import MorphTreeTX = Typing.MorphTreeTX;
  import Forest0 = Typing.Forest0;
  import Tree0 = Typing.Tree0;
  import Node0 = Typing.Node0;
  import CNode = core.CNode;
  import CTree = core.CTree;
  import CForest = core.CForest;

  export function IsForest<TNode extends Node0, TTree extends Tree0, TForest extends Forest0>(forest: TNode | TTree | TForest): forest is TForest;
  export function IsForest<TForest extends Forest0, T>(forest: TForest | T): forest is TForest;
  export function IsForest(forest: any): forest is CForest
  { return Object.getPrototypeOf(forest) === CForest.prototype; }

  export function IsTree<TNode extends Node0, TTree extends Tree0, TForest extends Forest0>(tree: TNode | TTree | TForest): tree is TTree;
  export function IsTree<TTree extends Tree0, T>(tree: TTree | T): tree is TTree;
  export function IsTree(tree: any): tree is CTree
  { return Object.getPrototypeOf(tree) === CTree.prototype; }

  export function IsNode<TNode extends Node0, TTree extends Tree0, TForest extends Forest0>(node: TNode | TTree | TForest): node is TNode;
  export function IsNode<TNode extends Node0, T>(node: TNode | T): node is TNode;
  export function IsNode(node: any): node is CNode
  { return Object.getPrototypeOf(node) === CNode.prototype; }

  /** fix all ref index before action: insert (after node at pos) or remove (subtree at pos) @returns for insert return insert point, for remove return remove count ram tree tree to fix @param pos pos of remove or insert @param shift when insert poasing postive number as amount of nodes to insert, form remove leave this efine @param aschild used for insert after node at pos. ture:as first child of node. false:as next peer of node */
  export function fixIndexBeforeInsertOrRemove(curNode: CNode, shift?: number, aschild?: boolean): number | undefined
  {
    let tree = curNode.tree;
    let nodes = tree.nodes;
    let nodeCount = nodes.length;
    let pos = curNode.index;
    let isInsert: boolean;
    let asPeer: boolean = true;
    if (shift !== undefined)
    {
      //insert
      if (shift <= 0) { return; } //inserting nothing
      isInsert = true;
      if (aschild !== undefined) { asPeer = !aschild; }

      if (pos >= nodeCount - 1)
      {
        //inert at end of nodes
        let p: CNode | undefined;
        if (asPeer) { p = curNode.parent; }
        else { p = curNode; }
        if (p !== undefined) { p.childrenID.push(nodeCount); }
        return;
      }
    } //remove
    else
    {
      if (pos >= nodeCount - 1)
      {
        //remove last
        let p = curNode.parent;
        if (p !== undefined) { p.childrenID.pop(); }
        return;
      } //insert at last or remove nothing no need to fix index
      let removeEndPt = subTreeEnd.call(curNode);
      if (removeEndPt === nodeCount)
      {
        //remove all node to the end
        forAcending.call(curNode, p => { p.childrenID = p.childrenID.filter(c => c < pos); });
        return;
      }
      shift = pos - removeEndPt;
      isInsert = false;
    }

    if (isInsert)//insert
    {
      if (asPeer)//insert as peer
      {
        //find right insert position
        pos = subTreeEnd.call(curNode);
        let curParent = curNode.parent;
        if (curParent !== undefined)
        {
          let childrenID = curParent.childrenID;
          let len = childrenID.length;
          for (let i = curNode.peerIndex + 1; i < len; i++)
          {
            nodes[childrenID[i]].peerIndex++; //fix peer's peerIndex;
            childrenID[i] += shift;
          }
          childrenID.insert(curNode.peerIndex + 1, pos);

          forAcending.call(curParent, (p, c) =>
          {
            //fix childIDs for from parent of parent up to root
            let childrenID = p.childrenID;
            let len = childrenID.length;
            for (let i = c.peerIndex + 1; i < len; i++)
            { childrenID[i] += shift!; }
          });
        }
      }
      else//insert as first child
      {
        //find right insert position
        pos += 1;
        let childrenID = curNode.childrenID;
        for (let i = 0, len = childrenID.length; i < len; i++)
        {
          nodes[childrenID[i]].peerIndex++;
          childrenID[i] += shift;
        } //fix peer's peerIndex;
        childrenID.unshift(pos);
        //childID will be fix in the following process dont do it here!!!!
        forAcending.call(curNode, (p, c) =>
        {
          //fix childIDs from parent up to root
          let childrenID = p.childrenID;
          let len = childrenID.length;
          for (let i = c.peerIndex + 1; i < len; i++)
          { childrenID[i] += shift!; }
        });
      }
    }
    else//remove
    {
      let curParent = curNode.parent;
      if (curParent !== undefined)
      {
        let childrenID = curParent.childrenID;
        childrenID.splice(curNode.peerIndex, 1);//remove child index

        for (let i = curNode.peerIndex, len = childrenID.length; i < len; i++)
        {
          nodes[childrenID[i]].peerIndex--;
          childrenID[i] += shift;
        }//fix peer's peerIndex;

        forAcending.call(curParent, (p, c) =>
        {
          //fix childIDs for from parent of parent up to root
          let childrenID = p.childrenID;
          for (let i = c.peerIndex + 1, len = childrenID.length; i < len; i++)
          { childrenID[i] += shift!; }
        });
      }
    }

    for (let i = isInsert ? pos : pos - shift; i < nodeCount; i++)
    {
      //fix parent id and childid for nodes after end of edit range
      let fixNode = nodes[i];
      fixNode.index += shift;
      let pid = fixNode.parentID!;
      if (pid > pos) { fixNode.parentID = pid + shift; }
      let childrenID = fixNode.childrenID;
      for (let i = 0, len = childrenID.length; i < len; i++)
      { childrenID[i] += shift; }
    }
    return isInsert ? pos : -shift; //for insert return insert point, for remove return remove count
  }

  /** go up tree untill root applying action, return 'break' in action to stop the process @param this current node @param action callback on parent up to root,urning 'break' will stop the process
   */
  export function forAcending(this: CNode, action: (parent: CNode, child: CNode) => void | ('break' | undefined)): void
  {
    let parent = this.parent;
    if (parent !== undefined && action(parent, this) !== 'break')
    { forAcending.call(parent, action); }
  }

  /** go down tree untill deepest child applying action, visit all child or by picker on each level @param this current node @param action callback on child go down tip @param picker optional picker returns numbers choose child by peer id */
  export function forDecending(this: CNode, action: (child: CNode, parent: CNode) => void, picker?: (parent: CNode) => ReadonlyArray<number> | "all" | "none"): void
  {
    let nodes = this.tree.nodes;
    let childrenID = this.childrenID;
    if (picker !== undefined)
    {
      let pick = picker(this);
      if (pick === "none") { return; }
      else if (pick !== "all")
      {
        childrenID = pick.reduce((cid, peer) =>
        {
          let c = this.childrenID[peer];
          if (c !== undefined) { cid.push(c); }
          return cid;
        }, new Array<number>());
      }
    }
    childrenID.forEach(id =>
    {
      if (id !== undefined)
      {
        let c = nodes[id];
        action(c, this);
        forDecending.call(c, action, picker);
      }
    });
  }

  /** get id of acending next peer, which is next peer=> if not peer of parent=> and so on. if this node has no next acending peer(this and parent up to root are  last peer) undefined is returned @param this current node */
  export function acendingNextPeerID(this: CNode): number | undefined
  {
    var subEnd: number | undefined = undefined;
    forAcending.call(this, (p, c) =>
    {
      subEnd = p.childrenID[c.peerIndex + 1];
      if (subEnd !== undefined) { return 'break'; }
    });
    return subEnd;
  }

  export function subTreeEnd(this: CNode): number
  {
    let subEnd = acendingNextPeerID.call(this);
    return subEnd === undefined ? this.tree.nodes.length : subEnd;
  }

  //export function subTreeRange(this: CNode): L.Range { return L.StartEnd(this.index, subTreeEnd.call(this)); }

  export function findChild<T extends object>(this: CNode, matcher: (node: CNode) => boolean, deep: boolean | undefined = true): Array<CNode & T>
  {
    let childs = (deep ? this.deepChildren : this.children) as Array<CNode & T>;
    return childs.filter(v => matcher(v), this);
  }

  export function findNode<T extends object>(this: CTree, matcher: (node: CNode) => boolean): Array<CNode & T>
  { return this.nodes.filter(v => matcher(v), this) as Array<CNode & T>; }

  //export function nodeCount(this: RawTree): number { return this.nodes.length; }

  export function isChildOf(this: CNode, n: CNode): boolean
  {
    let p = this.parent;
    while (p !== undefined)
    {
      if (n === p) { return true; }
      p = p.parent;
    }
    return false;
  }

  export function child(this: CNode, ...i: (number | 'last')[]): CNode | undefined
  {
    let ids = this.childrenID;
    let nodes = this.tree.nodes;
    let [curI, ...restI] = i;
    let curNd: CNode;
    if (L.IsNumber(curI))
    {
      if (curI < 0 || curI >= ids.length) { return undefined; }
      else { curNd = nodes[ids[curI]]; }
    }
    else
    {
      if (ids.length <= 0) { return undefined; }
      else { curNd = nodes[ids.last!]; }
    }
    return restI.length <= 0 ? curNd : child.call(curNd, ...restI);
  }

  export function polyAny(this: CNode | CTree, ...ext: any[]): any
  {
    //if (ext.length <= 1) { return this; }
    ext.forEach(e =>
    {
      e = L.IsFunction(e) ? e(this) : e;
      L.omitAssign(this, e, ['toString'], L.AssignFilter.extend);
      let s = e.toString;
      if (s !== undefined) { pushExtraLog.call(this, s); }
    });
    return this;
  }
  export function poly<T extends object>(this: CTree, ext?: T): L.Extend<CTree, T>;
  export function poly<T extends object>(this: CNode, ext?: (node: CNode) => T): L.Extend<CNode, T>;
  export function poly<T extends object[]>(this: CTree, ...ext: T): L.Extend<CTree, L.MergTupleType<T>>;
  export function poly<T extends object[]>(this: CNode, ...ext: T): L.Extend<CNode, L.MergTupleType<T>>;
  export function poly(this: CNode | CTree, ...ext: any[]): any
  { return polyAny.call(this, ...ext); }

  export function morph<T extends object>(this: CTree, ext?: T): MorphTreeTX<T>;
  export function morph<T extends object>(this: CNode, ext?: (node: CNode) => T): MorphNodeNX<T>;
  export function morph<T extends object[]>(this: CTree, ...ext: T): MorphTreeTX<L.MergTupleType<T>>;
  export function morph<T extends object[]>(this: CNode, ...ext: T): MorphNodeNX<L.MergTupleType<T>>;
  export function morph(this: CNode | CTree, ...ext: any[]): any
  { return polyAny.call(this, ...ext); }

  export function ctor(parent: CNode): CNode
  { return new core.CNode(parent.tree, parent.tree.nodes.length, parent.childrenID.length, parent.index); }

  export function attach(child: CNode, parent: CNode): void
  {
    let nodes = parent.tree.nodes;
    let insertPt: number | undefined = undefined;
    if (parent.childCount === 0) { insertPt = fixIndexBeforeInsertOrRemove(parent, 1, true); }
    else { insertPt = fixIndexBeforeInsertOrRemove(nodes[parent.childrenID.last!], 1, false); }
    if (insertPt === undefined) { nodes.push(child); }
    else { nodes.insert(insertPt, child); child.index = insertPt; }
  }

  export function Strip<T extends object>(node: CNode, ext: ((node: CNode) => T) | T): T;
  export function Strip<T extends object>(node: CTree, ext: ((node: CTree) => T) | T): T;
  export function Strip<T extends object>(node: CNode | CTree, ext: ((node: CNode | CTree) => T) | T): T
  { return L.NotFunction<T>(ext) ? ext : ext(node); }

  export function push(this: CNode | CTree): CNode;
  export function push<T extends object>(this: CNode | CTree, ext?: (node: CNode) => T): L.Extend<CNode, T>;
  export function push<T extends object>(this: CNode | CTree, morph: 'morph', ext?: (node: CNode) => T): MorphNodeNX<T>;
  export function push<T extends object[]>(this: CNode | CTree, ...ext: T): L.Extend<CNode, L.MergTupleType<T>>;
  export function push<T extends object[]>(this: CNode | CTree, morph: 'morph', ...ext: T): MorphNodeNX<L.MergTupleType<T>>;
  export function push(this: CNode | CTree, ...ext: any[]): any
  {
    let parent: CNode;
    let first = ext[0];
    if (first === 'morph') { ext.shift(); }
    if (IsTree(this)) { parent = this.root; }
    else { parent = this; }
    let node = ctor(parent);
    attach(node, parent);
    if (ext.length > 0) { polyAny.call(node, ...ext); }
    return node;
  }
  export function add(this: CNode): CNode;
  export function add(this: CTree): CTree;

  export function add<N extends object>(this: CNode, morph: 'morph', ext?: (node: CNode) => N): MorphNodeNX<N>;
  export function add<T extends object>(this: CTree, morph: 'morph', ext?: (node: CNode) => T): MorphTreeNX<T>;

  export function add<N extends object>(this: CNode, ext?: (node: CNode) => N): L.Extend<CNode, N>;
  export function add<T extends object>(this: CTree, ext?: (node: CNode) => T): L.Extend<CTree, T>;

  export function add<N extends object[]>(this: CNode, morph: 'morph', ...ext: N): MorphNodeNX<L.MergTupleType<N>>;
  export function add<T extends object[]>(this: CTree, morph: 'morph', ...ext: T): MorphTreeNX<L.MergTupleType<T>>;
  export function add<N extends object[]>(this: CNode, ...ext: N): L.Extend<CNode, L.MergTupleType<N>>;
  export function add<T extends object[]>(this: CTree, ...ext: T): L.Extend<CTree, L.MergTupleType<T>>;
  export function add(this: CNode | CTree, ...ext: any[]): any
  {
    let parent: CNode;
    let first = ext[0];
    if (first === 'morph') { ext.shift(); }
    if (IsTree(this)) { parent = this.root; }
    else { parent = this; }
    let node = ctor(parent);
    attach(node, parent);
    if (ext.length > 0) { polyAny.call(node, ...ext); }
    return this;
  }

  export function polySub<T extends object>(this: CNode, ext?: (node: CNode) => T): L.Extend<CNode, T>;
  export function polySub<T extends object[]>(this: CNode, ...ext: T): L.Extend<CNode, L.MergTupleType<T>>;
  export function polySub(this: CNode, ...ext: any[]): CNode
  {
    let nodes = this.tree.nodes;
    let range = this.subTreeRange;
    for (let i = range.start; i < range.end; i++) { nodes[i].poly(ext); }
    return this;
  }

  export function morphSub<T extends object>(this: CNode, ext?: (node: CNode) => T): MorphNodeNX<T>;
  export function morphSub<T extends object[]>(this: CNode, ...ext: T): MorphNodeNX<L.MergTupleType<T>>;
  export function morphSub(this: CNode, ...ext: any[]): any
  { return this.polySub(...ext); }

  // polymorph<N extends object>(nodeExt: (node: TNode) => N): TTree & MorphTreeN<N>;
  // polymorph<T extends object>(...ext: T[]): TTree & MorphTreeN<T>;
  // polymorph<N extends object, T extends object>(nodeExt: (node: TNode) => N, ...ext: T[]): TTree & MorphTree<N, T>;

  export function polymorph<N extends object, T extends object>(this: CTree): MorphTreeX<N, T>;
  export function polymorph<T extends object>(this: CTree, ext?: T): MorphTreeTX<T>;
  export function polymorph<N extends object>(this: CTree, nodeExt: (node: CNode) => N): MorphTreeNX<N>;
  export function polymorph<N extends object, T extends object>(this: CTree, nodeExt: (node: CNode) => N): MorphTreeX<N, T>;
  export function polymorph<N extends object, T extends object>(this: CTree, nodeExt: (node: CNode) => N, ext: T): MorphTreeX<N, T>;
  export function polymorph<N extends object, T extends object[]>(this: CTree, nodeExt: (node: CNode) => N, ...ext: T): MorphTreeX<N, L.MergTupleType<T>>;
  export function polymorph<T extends object[]>(this: CTree, ...ext: T): MorphTreeTX<L.MergTupleType<T>>;
  export function polymorph(this: CTree, ...ext: any[]): any
  {
    let nodes = this.nodes;
    let [firstExt, ...treeExt] = ext;
    if (L.IsFunction(firstExt))
    {
      polyAny.call(this, treeExt);
      for (let i = 1, len = nodes.length; i < len; i++)
      { nodes[i].poly(firstExt(nodes[i])); }
      return this;
    }
    else { polyAny.call(this, ...ext); }
    return this;
  }

  export function* NodeTriversGen(this: CNode | CTree): IterableIterator<CNode>
  {
    let [nodes, i, end] = IsTree(this) ? [this.nodes, 0, this.nodes.length] : [this.tree.nodes, this.index, subTreeEnd.call(this)];
    for (; i < end; i++) { yield nodes[i]; }
  }

  function refExtLog(this: CNode | CTree): IExtraLog
  { return ((IsTree(this) ? this.info : this.nodeInfo) as unknown) as IExtraLog; }

  function pushExtraLog(this: CNode | CTree, ...extralogs: Array<() => string>)
  {
    let extraLog = refExtLog.call(this);
    if (extraLog.extraLogs === undefined)
    { extraLog.extraLogs = extralogs; }
    else { extraLog.extraLogs.push(...extralogs); }
  }

  function CopyExtraLog(from: CNode | CTree, to: CNode | CTree): void
  {
    let extraLog = refExtLog.call(from);
    if (extraLog.extraLogs !== undefined)
    { refExtLog.call(to).extraLogs = extraLog.extraLogs.map(n => n); }
  }

  export function OverrideExtraLog(this: CNode | CTree, exlog: () => string): void
  {
    let extraLog = refExtLog.call(this);
    extraLog.extraLogs = [exlog];
  }

  export function extraLogStr(this: CTree | CNode): string
  {
    let log = '';
    let extraLog = refExtLog.call(this).extraLogs;
    if (extraLog !== undefined)
    {
      log += ':';
      for (let i = 0, len = extraLog.length; i < len; i++)
      { log += extraLog[i].call(this); }
    }
    return log;
  }

  export function simpleInfo(this: CNode): string
  { return `${this.isRoot ? 'R' : 'N'}.${this.index}`; }

  export function detailInfo(this: CNode): string
  {
    let out: string;
    if (this.isRoot) { out = `N${this.index}[R${this.parentID === undefined ? '' : '!!!' + this.parentID}.${this.peerIndex}]`; }
    else { out = `N${this.index}[${this.parentID === undefined ? '!!!' : 'N' + this.parentID}.${this.peerIndex}]`; }
    let childCount = this.childrenID.length;
    return childCount <= 0 ? out : out + `[C(#${childCount}):${this.childrenID.join(',')}]`;
  }

  interface IExtraLog { extraLogs: Array<() => string>; }

  export function nodeInfo(this: CNode, detail: boolean = true): string
  { return (detail ? detailInfo.call(this) : simpleInfo.call(this)) + extraLogStr.call(this); }

  export function treeInfo(this: CTree | CNode, detail: boolean = true, indent: string = '', lastChildID?: number): string
  {
    let [cur, out] = IsTree(this) ? [this.root, `Tree[#N:${this.nodeCount}]:${extraLogStr.call(this)}\r\n`] : [this, ''];
    if (cur === undefined) { return out + '[Empty]'; }
    if (lastChildID !== undefined)
    {
      out += '\r\n';
      if (cur.peerIndex === lastChildID)
      {
        out += indent + String.fromCharCode(9492, 9472);
        indent += '  ';
      }
      else
      {
        out += indent + String.fromCharCode(9500, 9472);
        indent += String.fromCharCode(9474) + ' ';
      }
    }
    out += cur.nodeInfo(detail);
    lastChildID = cur.childrenID.length - 1;
    if (lastChildID >= 0)
    {
      out = cur.children.reduce<string>((s, n) =>
      {
        if (n === undefined) { s += '[Error:Undefinded Child]'; }
        else { s += treeInfo.call(n, detail, indent, lastChildID); }
        return s;
      }, out);
    }
    return out;
  }

  export function remove(this: CNode): CNode[]
  {
    let nodes = this.tree.nodes;
    let start = this.index;
    let removeCount = fixIndexBeforeInsertOrRemove(this);
    return removeCount === undefined ? [] : nodes.splice(start, removeCount);
  }

  export function merg(this: CTree | CNode, sub: CNode): CNode;
  export function merg(this: CTree | CNode, sub: CNode, tartPeerIdx?: number): CNode;
  export function merg(this: CTree | CNode, sub: CTree, cloneSub?: boolean): CNode;
  export function merg(this: CTree | CNode, sub: CTree, tartPeerIdx?: number, cloneSub?: boolean): CNode;
  export function merg(this: CTree | CNode, sub: CTree | CNode, tartPeerIdx: number | undefined | boolean = undefined, cloneSub: boolean = true): CNode
  {
    if (sub === undefined) { throw new Error('Merg Child is undefined'); }
    let [srcSubRoot, srcSubNodes, srcSubTree] = IsTree(sub) ? [sub.root, sub.nodes, sub] : [sub, sub.tree.nodes, sub.tree];
    if (srcSubRoot === undefined) { throw new Error('Merg Child tree is empty'); }
    let subIsNode = !srcSubRoot.isRoot;
    let subTreeCount = srcSubRoot.subTreeRange.length;

    let [tarParent, tarTree, tarNodes] = IsTree(this) ? [this.root, this, this.nodes] : [this, this.tree, this.tree.nodes];
    let tarPeerCount: number;
    let tartinsertPt: number | undefined;
    let tarSubRtIndex: number;
    let tarParentIdx: number | undefined;
    if (tarParent !== undefined)
    {
      tarParentIdx = tarParent.index;
      tarPeerCount = tarParent.childCount;
      if (tartPeerIdx === undefined) { tartPeerIdx = tarPeerCount; }
      else if (L.IsBoolean(tartPeerIdx)) { [cloneSub, tartPeerIdx] = [tartPeerIdx, tarPeerCount]; }
      else { tartPeerIdx = Math.min(tarPeerCount, tartPeerIdx); }

      if (tartPeerIdx === 0) { tartinsertPt = fixIndexBeforeInsertOrRemove(tarParent, subTreeCount, true); }
      else { tartinsertPt = fixIndexBeforeInsertOrRemove(tarNodes[tarParent.childrenID[tartPeerIdx - 1]], subTreeCount, false); }

      if (tartinsertPt === undefined) { tarSubRtIndex = tarNodes.length; }
      else { tarSubRtIndex = tartinsertPt; }
    }
    else //target tree is empty!!!
    {
      if (L.IsBoolean(tartPeerIdx)) { cloneSub = tartPeerIdx; }
      [tarPeerCount, tartinsertPt, tarSubRtIndex, tarParentIdx, tartPeerIdx] = [0, 0, 0, undefined, 0];
    }

    let src2tarOffet = tarSubRtIndex - srcSubRoot.index;
    let newSub: CNode;
    let newSubNodes: CNode[];
    cloneSub = cloneSub || subIsNode;
    if (cloneSub)
    {
      newSub = new core.CNode(tarTree, tarSubRtIndex, tartPeerIdx, tarParentIdx);
      newSubNodes = [newSub];
      for (let i = 1; i < subTreeCount; i++)
      {
        let sn = srcSubNodes[i];
        let nsn = new core.CNode(tarTree, i + tarSubRtIndex, sn.peerIndex, sn.parentID! + src2tarOffet);
        L.omitAssign(nsn, sn, ['toString'], L.AssignFilter.extend);
        CopyExtraLog(sn, nsn);
        newSubNodes.push(nsn);
      }
      for (let i = 0; i < subTreeCount; i++)
      { newSubNodes[i].childrenID = srcSubNodes[i].childrenID.map(i => i + src2tarOffet); }
    }
    else
    {
      newSub = srcSubRoot;
      newSubNodes = srcSubNodes;

      newSub.tree = tarTree;
      newSub.index = tarSubRtIndex;
      newSub.peerIndex = tartPeerIdx;
      newSub.parentID = tarParentIdx;
      for (let i = 1; i < subTreeCount; i++)
      {
        let newNode = srcSubNodes[i];
        newNode.tree = tarTree;
        newNode.index = i + tarSubRtIndex;
        newNode.parentID = newNode.parentID! + src2tarOffet;
      }
      for (let i = 0; i < subTreeCount; i++)
      { newSubNodes[i].childrenID = newSubNodes[i].childrenID.map(i => i + src2tarOffet); }
      srcSubTree.nodes = [];
    }
    if (tartinsertPt === undefined) { tarNodes.push(...newSubNodes); }
    else { tarNodes.insert(tartinsertPt, ...newSubNodes); }
    newSub.peerIndex = tartPeerIdx;
    return newSub;
  }

  const nodeBaseProps: Array<PropertyKey> = ['index', 'tree', 'peerIndex', 'parentID', 'childrenID'];
  const NodeBaseProps = L.asLiterals(['index', 'tree', 'peerIndex', 'parentID', 'childrenID']);
  type NodeBaseProps = L.MapLiteralArray<typeof NodeBaseProps, any>;

  export function RemapNodeID(remap: number[], ...oldIndexs: readonly number[]): number[]
  {
    return oldIndexs.reduce<number[]>((p, n) =>
    {
      let newId = remap.indexOf(n);
      if (newId >= 0) { p.push(newId); }
      return p;
    }, []);
  }

  export function clone(this: CNode | CTree, cleanTree: boolean = false, picker?: (oldNode: CNode) => boolean, remix?: (newNode: CNode, oldNode: CNode) => void): CTree
  {
    let [srctree, srcCur, srcNodes] = IsTree(this) ? [this, this.root, this.nodes] : [this.tree, this, this.tree.nodes];
    //let srcIsRoot = srcCur.isRoot;
    let newTree: CTree = new core.CTree(true);
    if (!cleanTree)
    {
      L.omitAssign(newTree, srctree, ['toString'], L.AssignFilter.extend);
      CopyExtraLog(srctree, newTree);
    }
    let newNodes: core.CNode[] = (newTree.nodes = []);

    let { start: offset, end, length } = srcCur.subTreeRange;
    let indexMap: number[] = [];
    for (let pos = offset; pos < end;)
    {
      let srcNode = srcNodes[pos];
      let picked = picker === undefined ? true : picker(srcNode);
      if (picked)
      {
        indexMap.push(pos - offset); //map from [sequncial-offeted-index](final index) to [jumped-offeted-index](corrupted by jump)
        let parentInex = srcNode.parentID === undefined ? undefined : srcNode.parentID - offset;
        let newNode = new core.CNode(newTree, newNodes.length, srcNode.peerIndex, parentInex);
        if (!cleanTree)
        {
          L.omitAssign(newNode, srcNode, ['toString'], L.AssignFilter.extend);
          CopyExtraLog(srcNode, newNode);
        }
        newNode.childrenID = srcNode.childrenID.map(c => c - offset);
        newNode.tree = newTree;
        newNodes.push(newNode);
        pos++;
      }
      else { pos = srcNode.subTreeRange.end; }//jump over branch, removed node's deepchild is also removed

    }
    length = newNodes.length;
    if (length === 0) { return newTree; }
    let newRoot = newNodes[0];
    newRoot.parentID = undefined;
    newRoot.peerIndex = 0;
    if (picker !== undefined)
    {
      newRoot.childrenID = RemapNodeID(indexMap, ...newRoot.childrenID);
      for (let i = 1, len = length; i < len; i++)
      {
        let newNode = newNodes[i];
        newNode.childrenID = RemapNodeID(indexMap, ...newNode.childrenID);
        if (newNode.parentID !== undefined) { newNode.parentID = RemapNodeID(indexMap, newNode.parentID)[0]; }
        let newP = newNode.parent;
        newNode.peerIndex = newP === undefined ? 0 : newP.childrenID.indexOf(newNode.index);
        if (newNode.peerIndex < 0) { throw new Error('Node is missing!!!'); }
      }
    }

    if (remix !== undefined)
    {
      let nodeData: NodeBaseProps = L.Any;
      for (let i = 0; i < length; i++)
      {
        let newNode = newNodes[i];
        L.pickAssign(nodeData, newNode, nodeBaseProps, L.AssignFilter.alter);
        remix(newNode, srcNodes[indexMap[i + offset]]);
        L.omitAssign(newNode, nodeData, ['toString'], L.AssignFilter.override);
      }
    }
    return newTree;
  }

  export function nameString(this: Name): string
  { return this.name === undefined ? '[n/a]' : this.name; }

  export function findByName<TNode extends Node0>(this: TNode, name: string | undefined): Array<TNode>;
  export function findByName<TTree extends Tree0>(this: TTree, name: string | undefined): Array<Typing.NodeType<TTree>>;
  export function findByName<TFores extends Forest0>(this: TFores, name: string | undefined): Array<Typing.NodeTypeFromForest<TFores>>;
  export function findByName(this: CNode | CTree | CForest, name: string | undefined): Array<CNode>
  {
    if (func.IsNode(this)) { return this.findChild(n => n.poly<Name>().name === name, true); }
    else if (func.IsTree(this)) { return this.findNode(n => n.poly<Name>().name === name); }
    else
    {
      let ret: Array<CNode> = [];
      let trees = this.trees;
      for (let i = 0, len = trees.length; i < len; i++)
      { ret.push(...trees[i].findNode(n => n.poly<Name>().name === name)); }
      return ret as any;
    }
  }
}

namespace Typing
{
  import CNode = core.CNode;
  import CTree = core.CTree;
  //---------------------------------------------------------------------------------------------------------------------------------
  interface INode2Tree<TTree>
  {
    readonly tree: TTree;
    setLogger(logger: () => string): void;
  }

  interface ITree2Node<TNode>
  {
    readonly nodes: readonly TNode[];
    readonly nodeCount: number;
    setLogger(logger: () => string): void;
  }

  /** interal node type checking utility */
  export interface Node0 extends INode2Tree<Tree0> { }

  /** interal tree type checking utility */
  export interface Tree0 extends ITree2Node<Node0> { }
  //---------------------------------------------------------------------------------------------------------------------------------
  interface INodeIdx
  {
    readonly index: number;
    readonly peerIndex: number;
    readonly parentID: number | undefined;
    readonly childrenID: readonly number[];
    readonly subTreeRange: L.Range;
  }
  //---------------------------------------------------------------------------------------------------------------------------------
  /** Readonly Node with Simplified API clearer when used with linter */
  export interface INodeS<TNode extends INodeS<TNode, TTree>, TTree extends ITreeS<TNode, TTree>> extends INodeIdx, INode2Tree<TTree>
  {
    readonly parent: TNode | undefined;
    readonly children: readonly TNode[];
    readonly deepChildren: readonly TNode[];
    readonly childCount: number;

    forAcending(action: (parent: TNode, child: TNode) => void | ('break' | undefined)): void;
    forDecending(action: (child: TNode, parent: TNode) => void, picker?: (parent: TNode) => ReadonlyArray<number> | "all" | "none"): void;

    child(...i: (number | 'last')[]): TNode | undefined;
    clone(cleanTree?: true, picker?: (oldNode: TNode) => boolean, remix?: (newNode: NodeX, oldNode: TNode) => void): TreeX;
    clone(cleanTree?: false, picker?: (oldNode: TNode) => boolean, remix?: (newNode: TNode, oldNode: TNode) => void): TTree;

    poly<N extends object>(): L.Extend<TNode, N>;
    morph<N extends object>(): MorphNodeS<L.Extend<ExtOverNodeN<TNode>, N>, ExtOverTreeT<TTree>>;
  }

  /** Readonly Tree with Simplified API clearer when used with linter */
  export interface ITreeS<TNode extends INodeS<TNode, TTree>, TTree extends ITreeS<TNode, TTree>> extends ITree2Node<TNode>
  {
    readonly root: TNode;
    readonly tail: TNode;

    clone<N extends object, T extends object>(cleanTree: true, picker?: (oldNode: TNode) => boolean, remix?: (newNode: MorphNodeX<N, T>, oldNode: TNode) => void): MorphTreeX<N, T>;
    clone(cleanTree?: true, picker?: (oldNode: TNode) => boolean, remix?: (newNode: NodeX, oldNode: TNode) => void): TreeX;
    clone(cleanTree: false, picker?: (oldNode: TNode) => boolean, remix?: (newNode: TNode, oldNode: TNode) => void): TTree;
    readonly nodeCount: number;

    poly<T extends object>(): L.Extend<TTree, T>;
    morph<T extends object>(): MorphTreeS<ExtOverNodeN<TNode>, L.Extend<ExtOverTreeT<TTree>, T>>;
    polymorph<N extends object, T extends object>(): MorphTreeS<L.Extend<ExtOverNodeN<TNode>, N>, L.Extend<ExtOverTreeT<TTree>, T>>;
  }

  //---------------------------------------------------------------------------------------------------------------------------------
  /** Standard Readonly Node with all readonly funcitons */
  export interface INode<TNode extends INode<TNode, TTree>, TTree extends ITree<TNode, TTree>> extends INodeIdx, INode2Tree<TTree>
  {
    readonly parent: TNode | undefined;
    readonly children: readonly TNode[];
    readonly deepChildren: readonly TNode[];
    readonly childCount: number;
    child(...i: (number | 'last')[]): TNode | undefined;

    clone<N extends object, T extends object>(cleanTree: true, picker?: (oldNode: TNode) => boolean, remix?: (newNode: MorphNodeX<N, T>, oldNode: TNode) => void): MorphTreeX<N, T>;
    clone(cleanTree?: true, picker?: (oldNode: TNode) => boolean, remix?: (newNode: NodeX, oldNode: TNode) => void): TreeX;
    clone(cleanTree?: false, picker?: (oldNode: TNode) => boolean, remix?: (newNode: TNode, oldNode: TNode) => void): TTree;

    readonly isRoot: boolean;
    readonly isBranch: boolean;
    readonly isLeaf: boolean;

    readonly depth: number;
    isChildOf(node: TNode): boolean;
    findChild(matcher: (node: TNode) => boolean, deep?: boolean | undefined): Array<TNode>;
    findChild<T extends object>(matcher: (node: TNode) => boolean, deep?: boolean | undefined): Array<TNode & T>;

    forAcending(action: (parent: TNode, child: TNode) => void | ('break' | undefined)): void;
    forDecending(action: (child: TNode, parent: TNode) => void, picker?: (parent: TNode) => ReadonlyArray<number> | "all" | "none"): void;

    subTreeInfo(detail?: boolean): string;
    nodeInfo(detail?: boolean): string;

    poly<N extends object>(): L.Extend<TNode, N>;
    morph<N extends object>(): MorphNode<L.Extend<ExtOverNodeN<TNode>, N>, ExtOverTreeT<TTree>>;
  }

  /** Standard Readonly Tree with all readonly funcitons */
  export interface ITree<TNode extends INode<TNode, TTree>, TTree extends ITree<TNode, TTree>> extends ITree2Node<TNode>
  {
    readonly root: TNode;
    readonly tail: TNode;

    clone<N extends object, T extends object>(cleanTree: true, picker?: (oldNode: TNode) => boolean, remix?: (newNode: MorphNodeX<N, T>, oldNode: TNode) => void): MorphTreeX<N, T>;
    clone(cleanTree?: true, picker?: (oldNode: TNode) => boolean, remix?: (newNode: NodeX, oldNode: TNode) => void): TreeX;
    clone(cleanTree?: false, picker?: (oldNode: TNode) => boolean, remix?: (newNode: TNode, oldNode: TNode) => void): TTree;
    readonly nodeCount: number;

    findNode(matcher: (node: TNode) => boolean): Array<TNode>;
    findNode<T extends object>(matcher: (node: TNode) => boolean): Array<TNode & T>;
    info(detail?: boolean): string;

    poly<T extends object>(): L.Extend<TTree, T>;
    morph<T extends object>(): MorphTreeS<ExtOverNodeN<TNode>, L.Extend<ExtOverTreeT<TTree>, T>>;
    polymorph<N extends object, T extends object>(): MorphTreeS<L.Extend<ExtOverNodeN<TNode>, N>, L.Extend<ExtOverTreeT<TTree>, T>>;
  }

  //---------------------------------------------------------------------------------------------------------------------------------

  /** Editable Node with all api */
  export interface INodeX<TNode extends INodeX<TNode, TTree>, TTree extends ITreeX<TNode, TTree>> extends INodeIdx, INode2Tree<TTree>
  {
    readonly parent: TNode | undefined;
    readonly children: readonly TNode[];
    readonly deepChildren: readonly TNode[];
    readonly childCount: number;
    child(...i: (number | 'last')[]): TNode | undefined;

    clone<N extends object, T extends object>(cleanTree: true, picker?: (oldNode: TNode) => boolean, remix?: (newNode: MorphNodeX<N, T>, oldNode: TNode) => void): MorphTreeX<N, T>;
    clone(cleanTree?: true, picker?: (oldNode: TNode) => boolean, remix?: (newNode: NodeX, oldNode: TNode) => void): TreeX;
    clone(cleanTree?: false, picker?: (oldNode: TNode) => boolean, remix?: (newNode: TNode, oldNode: TNode) => void): TTree;

    readonly isRoot: boolean;
    readonly isBranch: boolean;
    readonly isLeaf: boolean;

    readonly depth: number;
    isChildOf(node: TNode): boolean;
    findChild(matcher: (node: TNode) => boolean, deep?: boolean | undefined): Array<TNode>;
    findChild<T extends object>(matcher: (node: TNode) => boolean, deep?: boolean | undefined): Array<TNode & T>;

    forAcending(action: (parent: TNode, child: TNode) => void | ('break' | undefined)): void;
    forDecending(action: (child: TNode, parent: TNode) => void, picker?: (parent: TNode) => ReadonlyArray<number> | "all" | "none"): void;

    subTreeInfo(detail?: boolean): string;
    nodeInfo(detail?: boolean): string;

    push(): TNode;
    push<N extends object>(ext?: (node: TNode) => N): L.Extend<TNode, N>;
    push<N extends object>(morph: 'morph', ext?: (node: TNode) => N): MorphNodeX<L.Extend<ExtOverNodeN<TNode>, N>, ExtOverTreeT<TTree>>;
    push<N extends object[]>(morph: 'morph', ...ext: N): MorphNodeX<L.Extend<ExtOverNodeN<TNode>, L.MergTupleType<N>>, ExtOverTreeT<TTree>>;
    push<N extends object[]>(...ext: N): L.Extend<TNode, L.MergTupleType<N>>;

    add(): TNode;
    add<N extends object>(ext?: (node: TNode) => N): TNode;
    add<N extends object>(morph: 'morph', ext?: (node: TNode) => N): MorphNodeX<L.Extend<ExtOverNodeN<TNode>, N>, ExtOverTreeT<TTree>>;
    add<N extends object[]>(morph: 'morph', ...ext: N): MorphNodeX<L.Extend<ExtOverNodeN<TNode>, L.MergTupleType<N>>, ExtOverTreeT<TTree>>;
    add<N extends object[]>(...ext: N): TNode;

    merg<TSub extends Node0>(sub: TSub): TNode;
    merg<TSub extends Node0>(sub: TSub, tartPeerIdx?: number): TNode;
    merg<TSub extends Tree0>(sub: TSub, cloneSub?: boolean): TNode;
    merg<TSub extends Tree0>(sub: TSub, tartPeerIdx?: number, cloneSub?: boolean): TNode;

    remove(): TNode[];

    poly<N extends object>(): L.Extend<TNode, N>;
    poly<N extends object[]>(...ext: N): L.Extend<TNode, L.MergTupleType<N>>;
    poly<N extends object>(ext: (node: TNode) => N): L.Extend<TNode, N>;

    morph<N extends object>(): MorphNodeX<L.Extend<ExtOverNodeN<TNode>, N>, ExtOverTreeT<TTree>>;
    morph<N extends object[]>(...ext: N): MorphNodeX<L.Extend<ExtOverNodeN<TNode>, L.MergTupleType<N>>, ExtOverTreeT<TTree>>;
    morph<N extends object>(ext: (node: TNode) => N): MorphNodeX<L.Extend<ExtOverNodeN<TNode>, N>, ExtOverTreeT<TTree>>;

    polySub<N extends object>(): L.Extend<TNode, N>;
    polySub<N extends object[]>(...ext: N): L.Extend<TNode, L.MergTupleType<N>>;
    polySub<N extends object>(ext: (node: TNode) => N): L.Extend<TNode, N>;

    morphSub<N extends object>(): MorphNodeX<L.Extend<ExtOverNodeN<TNode>, N>, ExtOverTreeT<TTree>>;
    morphSub<N extends object[]>(...ext: N): MorphNodeX<L.Extend<ExtOverNodeN<TNode>, L.MergTupleType<N>>, ExtOverTreeT<TTree>>;
    morphSub<N extends object>(ext: (node: TNode) => N): MorphNodeX<L.Extend<ExtOverNodeN<TNode>, N>, ExtOverTreeT<TTree>>;
  }

  /** Editable Tree with all api */
  export interface ITreeX<TNode extends INodeX<TNode, TTree>, TTree extends ITreeX<TNode, TTree>> extends ITree2Node<TNode>
  {
    readonly root: TNode;
    readonly tail: TNode;

    clone<N extends object, T extends object>(cleanTree: true, picker?: (oldNode: TNode) => boolean, remix?: (newNode: MorphNodeX<N, T>, oldNode: TNode) => void): MorphTreeX<N, T>;
    clone(cleanTree?: true, picker?: (oldNode: TNode) => boolean, remix?: (newNode: NodeX, oldNode: TNode) => void): TreeX;
    clone(cleanTree?: false, picker?: (oldNode: TNode) => boolean, remix?: (newNode: TNode, oldNode: TNode) => void): TTree;
    readonly nodeCount: number;

    findNode(matcher: (node: TNode) => boolean): Array<TNode>;
    findNode<T extends object>(matcher: (node: TNode) => boolean): Array<TNode & T>;
    info(detail?: boolean): string;

    push(): TNode;
    push<N extends object>(ext?: (node: TNode) => N): L.Extend<TNode, N>;
    push<N extends object>(morph: 'morph', ext?: (node: TNode) => N): MorphNodeX<L.Extend<ExtOverNodeN<TNode>, N>, ExtOverTreeT<TTree>>;
    push<N extends object[]>(...ext: N): L.Extend<TNode, L.MergTupleType<N>>;
    push<N extends object[]>(morph: 'morph', ...ext: N): MorphNodeX<L.Extend<ExtOverNodeN<TNode>, L.MergTupleType<N>>, ExtOverTreeT<TTree>>;

    add(): TTree;
    add<N extends object>(ext?: (node: TNode) => N): TTree;
    add<N extends object>(morph: 'morph', ext?: (node: TNode) => N): MorphTreeX<L.Extend<ExtOverNodeN<TNode>, N>, ExtOverTreeT<TTree>>;
    add<N extends object[]>(...ext: N): TTree;
    add<N extends object[]>(morph: 'morph', ...ext: N): MorphTreeX<L.Extend<ExtOverNodeN<TNode>, L.MergTupleType<N>>, ExtOverTreeT<TTree>>;

    merg<TSub extends Node0>(sub: TSub): TNode;
    merg<TSub extends Node0>(sub: TSub, tartPeerIdx?: number): TNode;
    merg<TSub extends Tree0>(sub: TSub, cloneSub?: boolean): TNode;
    merg<TSub extends Tree0>(sub: TSub, tartPeerIdx?: number, cloneSub?: boolean): TNode;

    poly<T extends object>(): L.Extend<TTree, T>;
    poly<T extends object[]>(...ext: T): L.Extend<TTree, L.MergTupleType<T>>;

    morph<T extends object>(): MorphTreeX<ExtOverNodeN<TNode>, L.Extend<ExtOverTreeT<TTree>, T>>;
    morph<T extends object[]>(...ext: T): MorphTreeX<ExtOverNodeN<TNode>, L.Extend<ExtOverTreeT<TTree>, L.MergTupleType<T>>>;

    polymorph<N extends object, T extends object>(): MorphTreeX<L.Extend<ExtOverNodeN<TNode>, N>, L.Extend<ExtOverTreeT<TTree>, T>>;
    polymorph<N extends object>(nodeExt: (node: TNode) => N): MorphTreeX<L.Extend<ExtOverNodeN<TNode>, N>, ExtOverTreeT<TTree>>;
    polymorph<T extends object>(ext?: T): MorphTreeX<ExtOverNodeN<TNode>, L.Extend<ExtOverTreeT<TTree>, T>>;
    polymorph<N extends object, T extends object>(nodeExt: (node: TNode) => N, ext: T): MorphTreeX<L.Extend<ExtOverNodeN<TNode>, N>, L.Extend<ExtOverTreeT<TTree>, T>>;
    polymorph<N extends object, T extends object>(nodeExt: (node: TNode) => N): MorphTreeX<L.Extend<ExtOverNodeN<TNode>, N>, L.Extend<ExtOverTreeT<TTree>, T>>;
    polymorph<T extends object[]>(...ext: T): MorphTreeX<ExtOverNodeN<TNode>, L.Extend<ExtOverTreeT<TTree>, L.MergTupleType<T>>>;
  }
  //---------------------------------------------------------------------------------------------------------------------------------

  /** Unsafe raw Node with indexing access */
  export interface IRawNode<TNode extends IRawNode<TNode, TTree>, TTree extends IRawTree<TNode, TTree>> extends INode2Tree<TTree>, INodeIdx { }

  /** Unsafe raw Tree with indexing access */
  export interface IRawTree<TNode extends IRawNode<TNode, TTree>, TTree extends IRawTree<TNode, TTree>> extends ITree2Node<TNode> { }

  //---------------------------------------------------------------------------------------------------------------------------------

  interface IPolyNodes<N extends object, T extends object, TNode extends IPolyNodes<N, T, TNode, TTree>, TTree extends IPolyTreeS<N, T, TNode, TTree>>
    extends INodeS<L.Extend<TNode, N>, L.Extend<TTree, T>> { }

  interface IPolyTreeS<N extends object, T extends object, TNode extends IPolyNodes<N, T, TNode, TTree>, TTree extends IPolyTreeS<N, T, TNode, TTree>>
    extends ITreeS<L.Extend<TNode, N>, L.Extend<TTree, T>> { }

  interface IPolyNode<N extends object, T extends object, TNode extends IPolyNode<N, T, TNode, TTree>, TTree extends IPolyTree<N, T, TNode, TTree>>
    extends INode<L.Extend<TNode, N>, L.Extend<TTree, T>> { }

  interface IPolyTree<N extends object, T extends object, TNode extends IPolyNode<N, T, TNode, TTree>, TTree extends IPolyTree<N, T, TNode, TTree>>
    extends ITree<L.Extend<TNode, N>, L.Extend<TTree, T>> { }

  interface IPolyNodeX<N extends object, T extends object, TNode extends IPolyNodeX<N, T, TNode, TTree>, TTree extends IPolyTreeX<N, T, TNode, TTree>>
    extends INodeX<L.Extend<TNode, N>, L.Extend<TTree, T>> { }

  interface IPolyTreeX<N extends object, T extends object, TNode extends IPolyNodeX<N, T, TNode, TTree>, TTree extends IPolyTreeX<N, T, TNode, TTree>>
    extends ITreeX<L.Extend<TNode, N>, L.Extend<TTree, T>> { }

  //---------------------------------------------------------------------------------------------------------------------------------

  /** Unsafe raw Node with indexing access */
  export interface RawNode extends IRawNode<RawNode, RawTree> { }

  /** Unsafe raw Tree with indexing access */
  export interface RawTree extends IRawTree<RawNode, RawTree> { }

  /** Readonly Node with Simplified API clearer when used with linter */
  export interface NodeS extends INodeS<NodeS, TreeS> { }

  /** Readonly Tree with Simplified API clearer when used with linter */
  export interface TreeS extends ITreeS<NodeS, TreeS> { }

  /** Readonly Node with Simplified API clearer when used with linter */
  export interface Node extends INode<Node, Tree> { }

  /** Readonly Tree with Simplified API clearer when used with linter */
  export interface Tree extends ITree<Node, Tree> { }

  /** Editable Node with all api */
  export interface NodeX extends INodeX<NodeX, TreeX> { }

  /** Editable Tree with all api */
  export interface TreeX extends ITreeX<NodeX, TreeX> { }

  //---------------------------------------------------------------------------------------------------------------------------------
  interface PolyNodeS<N extends object, T extends object> extends IPolyNodes<N, T, PolyNodeS<N, T>, PolyTreeS<N, T>> { }
  interface PolyTreeS<N extends object, T extends object> extends IPolyTreeS<N, T, PolyNodeS<N, T>, PolyTreeS<N, T>> { }
  interface PolyNode<N extends object, T extends object> extends IPolyNode<N, T, PolyNode<N, T>, PolyTree<N, T>> { }
  interface PolyTree<N extends object, T extends object> extends IPolyTree<N, T, PolyNode<N, T>, PolyTree<N, T>> { }
  interface PolyNodeX<N extends object, T extends object> extends IPolyNodeX<N, T, PolyNodeX<N, T>, PolyTreeX<N, T>> { }
  interface PolyTreeX<N extends object, T extends object> extends IPolyTreeX<N, T, PolyNodeX<N, T>, PolyTreeX<N, T>> { }
  //---------------------------------------------------------------------------------------------------------------------------------

  /** simple node with extended node and tree type */
  export type MorphNodeS<N extends object, T extends object> = PolyNodeS<N, T> & N;

  /** simple node with extended node type */
  export type MorphNodeNS<N extends object> = PolyNodeS<N, object> & N;

  /** simple node with extended tree type */
  export type MorphNodeTS<T extends object> = PolyNodeS<object, T>;

  /** simple tree with extended node and tree type */
  export type MorphTreeS<N extends object, T extends object> = PolyTreeS<N, T> & T;

  /** simple tree with extended node type */
  export type MorphTreeNS<N extends object> = PolyTreeS<N, object>;

  /** simple tree with extended tree type */
  export type MorphTreeTS<T extends object> = PolyTreeS<object, T> & T;

  //---------------------------------------------------------------------------------------------------------------------------------

  /** standard node with extended node and tree type */
  export type MorphNode<N extends object, T extends object> = PolyNode<N, T> & N;

  /** standard node with extended node type */
  export type MorphNodeN<N extends object> = PolyNode<N, object> & N;

  /** standard node with extended tree type */
  export type MorphNodeT<T extends object> = PolyNode<object, T>;

  /** standard tree with extended node and tree type */
  export type MorphTree<N extends object, T extends object> = PolyTree<N, T> & T;

  /** standard tree with extended node type */
  export type MorphTreeN<N extends object> = PolyTree<N, object>;

  /** standard tree with extended tree type */
  export type MorphTreeT<T extends object> = PolyTree<object, T> & T;

  //---------------------------------------------------------------------------------------------------------------------------------

  /** editable node with extended node and tree type */
  export type MorphNodeX<N extends object, T extends object> = PolyNodeX<N, T> & N;

  /** editable node with extended node type */
  export type MorphNodeNX<N extends object> = PolyNodeX<N, object> & N;

  /** editable node with extended tree type */
  export type MorphNodeTX<T extends object> = PolyNodeX<object, T>;

  /** editable tree with extended node and tree type */
  export type MorphTreeX<N extends object, T extends object> = PolyTreeX<N, T> & T;

  /** editable tree with extended node type */
  export type MorphTreeNX<N extends object> = PolyTreeX<N, object>;

  /** editable tree with extended tree type */
  export type MorphTreeTX<T extends object> = PolyTreeX<object, T> & T;

  //---------------------------------------------------------------------------------------------------------------------------------
  /** typeing utility to find poly extention from node type*/
  export type ExtOverNodeN<TNode extends object> = Omit<TNode, keyof CNode>;

  /** typeing utility to find poly extention from node packed in tree type*/
  export type ExtOverNodeT<TTree extends object> = Omit<NodeType<TTree>, keyof CNode>;

  /** typeing utility to find poly extention from tree packed in node type*/
  export type ExtOverTreeN<TNode extends object> = Omit<TreeType<TNode>, keyof CTree>;

  /** typeing utility to find poly extention from tree type*/
  export type ExtOverTreeT<TTree extends object> = Omit<TTree, keyof CTree>;

  /** typeing utility to find node type packed in tree type*/
  export type NodeType<TTree> = TTree extends { root: infer TNode } ? (TNode extends Node0 ? TNode : never) : never;

  /** typeing utility to find tree type packed in node type*/
  export type TreeType<TNode> = TNode extends { tree: infer TTree } ? (TTree extends Tree0 ? TTree : never) : never;

  //export type NodeTypeFromForest<TForest extends object> = TForest extends { trees: infer TTree } ? (TNode extends object ? TNode : never) : never;
  export type TreeTypeFromForest<TForest> = TForest extends { trees: infer ATree } ? (L.ArrayElemType<ATree> extends Tree0 ? L.ArrayElemType<ATree> : never) : never;
  export type NodeTypeFromForest<TForest> = TForest extends { trees: infer ATree } ? NodeType<L.ArrayElemType<ATree>> : never;

  //---------------------------------------------------------------------------------------------------------------------------------

  /** reduce any deep morph tree/node type to simple type keep the deep morph */
  export type Simplify<O extends Tree0 | Node0> = O extends Node0 ? MorphNodeS<ExtOverNodeN<O>, ExtOverTreeN<O>> : MorphTreeS<ExtOverNodeT<O>, ExtOverTreeT<O>>;

  /** set any deep morph tree/node type to standard type keep the deep morph */
  export type Nomalize<O extends Tree0 | Node0> = O extends Node0 ? MorphNode<ExtOverNodeN<O>, ExtOverTreeN<O>> : MorphTree<ExtOverNodeT<O>, ExtOverTreeT<O>>;

  /** convert any deep morph tree/node type to editable type keep the deep morph */
  export type Edit<O extends Tree0 | Node0> = O extends Node0 ? MorphNodeX<ExtOverNodeN<O>, ExtOverTreeN<O>> : MorphTreeX<ExtOverNodeT<O>, ExtOverTreeT<O>>;

  /** reduce any deep morph node to simple node keep the deep morph */
  export function Simplify<N extends Node0>(node: N): Simplify<N>;
  /** reduce any deep morph tree to simple tree keep the deep morph */
  export function Simplify<T extends Tree0>(tree: T): Simplify<T>;
  export function Simplify<O extends Tree0 | Node0>(obj: O): any
  { return obj as any; }

  /** set any deep morph node to standard node keep the deep morph */
  export function Nomalize<N extends Node0>(node: N): Nomalize<N>;
  /** set any deep morph tree to standard tree keep the deep morph */
  export function Nomalize<T extends Tree0>(tree: T): Nomalize<T>;
  export function Nomalize<O extends Tree0 | Node0>(obj: O): any
  { return obj as any; }

  /** convert any deep morph node to editable node keep the deep morph */
  export function Edit<N extends Node0>(node: N): Edit<N>;
  /** convert any deep morph tree to editable tree keep the deep morph */
  export function Edit<T extends Tree0>(tree: T): Edit<T>;
  export function Edit<O extends Tree0 | Node0>(obj: O): any
  { return obj as any; }

  export interface IForest<TTree extends Tree0>
  {
    trees: Array<TTree>;
    polymorph<N extends object>(): IForest<MorphTreeX<L.Extend<ExtOverNodeT<TTree>, N>, ExtOverTreeT<TTree>>>;
    polymorph<N extends object, T extends object>(): IForest<MorphTreeX<L.Extend<ExtOverNodeT<TTree>, N>, L.Extend<ExtOverTreeT<TTree>, T>>>;
    polymorph<N extends object, T extends object, F extends object>(): L.Extend<IForest<MorphTreeX<L.Extend<ExtOverNodeT<TTree>, N>, L.Extend<ExtOverTreeT<TTree>, T>>>, F>;

    polymorph<F extends object>(forestExt: F): L.Extend<IForest<TTree>, F>;

    polymorph<T extends object, F extends object>(treeExt: (node: CTree) => T, forestExt: F): L.Extend<IForest<MorphTreeX<ExtOverNodeT<TTree>, L.Extend<ExtOverTreeT<TTree>, T>>>, F>;

    polymorph<N extends object, T extends object, F extends object>(
      nodeExt: (node: CNode) => N,
      treeExt: (node: CTree) => T,
      forestExt: F,
    ): L.Extend<IForest<MorphTreeX<L.Extend<ExtOverNodeT<TTree>, N>, L.Extend<ExtOverTreeT<TTree>, T>>>, F>;

    polymorph<N extends object, T extends object, F extends object[]>(
      nodeExt: (node: CNode) => N,
      treeExt: (node: CTree) => T,
      ...forestExt: F
    ): L.Extend<IForest<MorphTreeX<L.Extend<ExtOverNodeT<TTree>, N>, L.Extend<ExtOverTreeT<TTree>, T>>>, L.MergTupleType<F>>;

    polymorph<T extends object, F extends object[]>(
      treeExt: (node: CTree) => T,
      ...forestExt: F
    ): L.Extend<IForest<MorphTreeX<ExtOverNodeT<TTree>, L.Extend<ExtOverTreeT<TTree>, T>>>, L.MergTupleType<F>>;

    polymorph<F extends object[]>(...forestExt: F): L.Extend<IForest<TTree>, L.MergTupleType<F>>;
  }

  export type Forest0 = IForest<Tree0>;
  export type Forest = IForest<TreeX>;
  export type ForestN<N extends object> = IForest<MorphTreeNX<N>>;
  export type ForestT<TTree extends Tree0> = IForest<TTree>;
  export type ForestF<F extends object> = L.Extend<IForest<TreeX>, F>;
  export type ForestNT<N extends object, T extends object> = IForest<MorphTreeX<N, T>>;
  export type ForestNF<N extends object, F extends object> = L.Extend<IForest<MorphTreeNX<N>>, F>;
  export type ForestTF<TTree extends Tree0, F extends object> = L.Extend<IForest<TTree>, F>;
  export type ForestNTF<N extends object, T extends object, F extends object> = L.Extend<IForest<MorphTreeX<N, T>>, F>;
}

export function Tree(mode: 'Simple'): Typing.TreeS;
export function Tree(mode: 'Reandonly'): Typing.Tree;
export function Tree(mode: 'Editable'): Typing.TreeX;
export function Tree(): Typing.TreeX;

export function Tree<N extends object, T extends object>(mode: 'Simple', nodeExt?: (node: any) => N, treeExt?: T): Typing.MorphTreeS<N, T>;
export function Tree<N extends object, T extends object>(mode: 'Reandonly', nodeExt?: (node: any) => N, treeExt?: T): Typing.MorphTree<N, T>;
export function Tree<N extends object, T extends object>(mode: 'Editable', nodeExt?: (node: any) => N, treeExt?: T): Typing.MorphTreeX<N, T>;
export function Tree<N extends object, T extends object>(nodeExt?: (node: any) => N, treeExt?: T): Typing.MorphTreeX<N, T>;

export function Tree<T extends object>(mode: 'Simple', treeExt?: T): Typing.MorphTreeTS<T>;
export function Tree<T extends object>(mode: 'Reandonly', treeExt?: T): Typing.MorphTreeT<T>;
export function Tree<T extends object>(mode: 'Editable', treeExt?: T): Typing.MorphTreeTX<T>;
export function Tree<T extends object>(treeExt?: T): Typing.MorphTreeTX<T>;

export function Tree<N extends object, T extends object>(mode: 'Simple', treeExt?: T): Typing.MorphTreeS<N, T>;
export function Tree<N extends object, T extends object>(mode: 'Reandonly', treeExt?: T): Typing.MorphTree<N, T>;
export function Tree<N extends object, T extends object>(mode: 'Editable', treeExt?: T): Typing.MorphTreeX<N, T>;
export function Tree<N extends object, T extends object>(treeExt?: T): Typing.MorphTreeX<N, T>;

export function Tree<N extends object, T extends object[]>(mode: 'Simple', nodeExt: (node: any) => N, ...treeExts: T): Typing.MorphTreeS<N, L.MergTupleType<T>>;
export function Tree<N extends object, T extends object[]>(mode: 'Reandonly', nodeExt: (node: any) => N, ...treeExts: T): Typing.MorphTree<N, L.MergTupleType<T>>;
export function Tree<N extends object, T extends object[]>(mode: 'Editable', nodeExt: (node: any) => N, ...treeExts: T): Typing.MorphTreeX<N, L.MergTupleType<T>>;
export function Tree<N extends object, T extends object[]>(nodeExt: (node: any) => N, ...treeExts: T): Typing.MorphTreeX<N, L.MergTupleType<T>>;

export function Tree<T extends object[]>(mode: 'Simple', ...treeExt: T): Typing.MorphTreeTS<L.MergTupleType<T>>;
export function Tree<T extends object[]>(mode: 'Reandonly', ...treeExt: T): Typing.MorphTreeT<L.MergTupleType<T>>;
export function Tree<T extends object[]>(mode: 'Editable', ...treeExt: T): Typing.MorphTreeTX<L.MergTupleType<T>>;
export function Tree<T extends object[]>(...treeExt: T): Typing.MorphTreeTX<L.MergTupleType<T>>;
export function Tree(...ext: any[]): any
{
  let t = new core.CTree(false);
  if (ext.length > 0)
  {
    let [first, ...rest] = ext;
    if (first === 'Simple' || first === 'Reandonly' || first === 'Editable')
    {
      if (rest.length <= 0) { return t; }
      [first, ...rest] = rest;
    }
    if (L.IsFunction(first))
    {
      let nx = first(t.root);
      t.root.morph(nx);
      t.morph(...rest);
    }
    else { t.morph(...ext); }
  }
  return t;
}

export namespace Tree
{
  export import CNode = core.CNode;
  export import CTree = core.CTree;

  export import IsNode = func.IsNode;
  export import IsTree = func.IsTree;
  export import IsForest = func.IsForest;

  export import Edit = Typing.Edit;
  export import Simplify = Typing.Simplify;
  export import Nomalize = Typing.Nomalize;

  export import NodeS = Typing.NodeS;
  export import TreeS = Typing.TreeS;

  export import Node = Typing.Node;
  export import Tree = Typing.Tree;

  export import NodeX = Typing.NodeX;
  export import TreeX = Typing.TreeX;

  export import MorphNodeS = Typing.MorphNodeS;
  export import MorphNodeNS = Typing.MorphNodeNS;
  export import MorphNodeTS = Typing.MorphNodeTS;

  export import MorphTreeS = Typing.MorphTreeS;
  export import MorphTreeTS = Typing.MorphTreeTS;
  export import MorphTreeNS = Typing.MorphTreeNS;

  export import MorphNode = Typing.MorphNode;
  export import MorphNodeN = Typing.MorphNodeN;
  export import MorphNodeT = Typing.MorphNodeT;

  export import MorphTree = Typing.MorphTree;
  export import MorphTreeT = Typing.MorphTreeT;
  export import MorphTreeN = Typing.MorphTreeN;

  export import MorphNodeX = Typing.MorphNodeX;
  export import MorphNodeNX = Typing.MorphNodeNX;
  export import MorphNodeTX = Typing.MorphNodeTX;

  export import MorphTreeX = Typing.MorphTreeX;
  export import MorphTreeTX = Typing.MorphTreeTX;
  export import MorphTreeNX = Typing.MorphTreeNX;

  export import ExtOverNodeN = Typing.ExtOverNodeN;
  export import ExtOverNodeT = Typing.ExtOverNodeT;
  export import ExtOverTreeN = Typing.ExtOverTreeN;
  export import ExtOverTreeT = Typing.ExtOverTreeT;

  export import NodeType = Typing.NodeType;
  export import TreeType = Typing.TreeType;
}

export function Forest(): Typing.Forest;
export function Forest<F extends object>(fExt: F): Typing.ForestF<F>;
export function Forest<N extends object, T extends object>(): Typing.ForestNT<N, T>;
export function Forest<N extends object, T extends object, F extends object>(): Typing.ForestNTF<N, T, F>;
export function Forest<N extends object, F extends object>(fExt: F): Typing.ForestNF<N, F>;
export function Forest<N extends object, T extends object, F extends object>(fExt: F): Typing.ForestNTF<N, T, F>;
export function Forest<N extends object, F extends object[]>(...fExt: F): Typing.ForestNF<N, L.MergTupleType<F>>;
export function Forest<N extends object, T extends object, F extends object[]>(...fExt: F): Typing.ForestNTF<N, T, L.MergTupleType<F>>;
export function Forest<F extends object[]>(...fExt: F): Typing.ForestF<L.MergTupleType<F>>;
export function Forest(...fExt: object[]): any
{
  let f = new core.CForest();
  fExt.forEach(x => L.assign(f, x, L.AssignFilter.extend));
  return f as any;
}

export namespace Forest
{
  //export import CForest = core.CForest;
  export import Forest = Typing.Forest;
  export import ForestF = Typing.ForestF;
  export import ForestT = Typing.ForestT;
  export import ForestTF = Typing.ForestTF;
  export import ForestNT = Typing.ForestNT;
  export import ForestNTF = Typing.ForestNTF;
  export import ForestN = Typing.ForestN;
  export import ForestNF = Typing.ForestNF;

  export import IsNode = func.IsNode;
  export import IsTree = func.IsTree;
  export import IsForest = func.IsForest;
}

export interface Name
{
  name: string | undefined;
  findByName(key: string | undefined): Array<Typing.MorphNode<Name, Name>>;
  findByName<TNode extends Typing.Node0>(name: string | undefined): Array<TNode>;
  findByName<TTree extends Typing.Tree0>(name: string | undefined): Array<Typing.NodeType<TTree>>;
  findByName<TFores extends Typing.Forest0>(name: string | undefined): Array<Typing.NodeTypeFromForest<TFores>>;
  toString(): string;
}

export function Name(name: string | undefined): Name
{ return { name, findByName: func.findByName, toString: func.nameString }; }

export namespace Name
{
  export function NamedTree(rootName?: string, treeName?: string): Typing.MorphTreeX<Name, Name>
  { return Tree("Editable", n => Name(rootName), Name(treeName)); }

  export type NamedTreeS = Typing.MorphTreeS<Name, Name>;
  export type NamedTreeMS<N extends object, T extends object> = Typing.MorphTreeS<Name & N, Name & T>;
  export type NamedTreeMNS<N extends object> = Typing.MorphTreeS<Name & N, Name>;
  export type NamedTreeMTS<T extends object> = Typing.MorphTreeS<Name & T, Name>;

  export type NamedTree = Typing.MorphTree<Name, Name>;
  export type NamedTreeM<N extends object, T extends object> = Typing.MorphTreeS<Name & N, Name & T>;
  export type NamedTreeMN<N extends object> = Typing.MorphTree<Name & N, Name>;
  export type NamedTreeMT<T extends object> = Typing.MorphTree<Name, Name & T>;

  export type NamedTreeX = Typing.MorphTreeX<Name, Name>;
  export type NamedTreeMX<N extends object, T extends object> = Typing.MorphTreeX<Name & N, Name & T>;
  export type NamedTreeMNX<N extends object> = Typing.MorphTreeX<Name & N, Name>;
  export type NamedTreeMTX<T extends object> = Typing.MorphTreeX<Name, Name & T>;

  export type NamedNodeS = Typing.MorphNodeS<Name, Name>;
  export type NamedNodeMS<N extends object, T extends object> = Typing.MorphNodeS<Name & N, Name & T>;
  export type NamedNodeMNS<N extends object> = Typing.MorphNodeS<Name & N, Name>;
  export type NamedNodeMTS<T extends object> = Typing.MorphNodeS<Name, Name & T>;

  export type NamedNode = Typing.MorphNode<Name, Name>;
  export type NamedNodeM<N extends object, T extends object> = Typing.MorphNode<Name & N, Name & T>;
  export type NamedNodeMN<N extends object> = Typing.MorphNode<Name & N, Name>;
  export type NamedNodeMT<T extends object> = Typing.MorphNode<Name, Name & T>;

  export type NamedNodeX = Typing.MorphNodeX<Name, Name>;
  export type NamedNodeMX<N extends object, T extends object> = Typing.MorphNodeX<Name & N, Name & T>;
  export type NamedNodeMNX<N extends object> = Typing.MorphNodeX<Name & N, Name>;
  export type NamedNodeMTX<T extends object> = Typing.MorphNodeX<Name, Name & T>;

  export type NamedForest = Typing.ForestNTF<Name, Name, Name>;
}

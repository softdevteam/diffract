// Copyright (c) 2017 King's College London
// created by the Software Development Team <http://soft-dev.org/>
//
// The Universal Permissive License (UPL), Version 1.0
//
// Subject to the condition set forth below, permission is hereby granted to any
// person obtaining a copy of this software, associated documentation and/or
// data (collectively the "Software"), free of charge and under any and all
// copyright rights in the Software, and any and all patent rights owned or
// freely licensable by each licensor hereunder covering either (i) the
// unmodified Software as contributed to or provided by such licensor, or (ii)
// the Larger Works (as defined below), to deal in both
//
// (a) the Software, and
// (b) any piece of software and/or hardware listed in the lrgrwrks.txt file
// if one is included with the Software (each a "Larger Work" to which the Software
// is contributed by such licensors),
//
// without restriction, including without limitation the rights to copy, create
// derivative works of, display, perform, and distribute the Software and make,
// use, sell, offer for sale, import, export, have made, and have sold the
// Software and the Larger Work(s), and to sublicense the foregoing rights on
// either these or other terms.
//
// This license is subject to the following condition: The above copyright
// notice and either this complete permission notice or at a minimum a reference
// to the UPL must be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#![warn(missing_docs)]

use std::any::Any;
use std::boxed::Box;
use std::clone::Clone;
use std::fmt;

use ast::{Arena, ArenaError, ArenaResult, NodeId, SrcNodeId};
use chawathe98_matcher::MappingStoreGraph;
use emitters::RenderJson;
use matchers::MappingStore;
use patch::Patch;

/// Type of action.
///
/// Only used where the information related to actions can safely be discarded.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ActionType {
    /// Delete a node in a 'src' arena.
    DELETE,
    /// Insert a new node into the 'src' arena.
    INSERT,
    /// Move an existing node.
    MOVE,
    /// Update the text or label associated with a node.
    UPDATE,
    /// Copy takes the node or subtree 'src tree' and and copies it into 'dst tree'.
    COPY,
    /// GLUE takes a subtree/node 'src tree' and inserts it into 'dst tree'
    GLUE
}

/// Apply an action to an AST node.
pub trait ApplyAction<T: Clone + fmt::Debug + Eq + PartialEq + ToString>:
    fmt::Debug + Patchify<T> + RenderJson
{
    /// Apply an action to an AST.
    fn apply(&mut self, arena: &mut Arena<T, SrcNodeId>) -> ArenaResult;
    /// Test for equality, avoiding a recursive call to `eq()`.
    fn is_eq(&self, other: &ApplyAction<T>) -> bool;
    /// Cast to `Any` type.
    fn as_any(&self) -> &Any;
}

impl<T: Clone + fmt::Debug + Eq + PartialEq + ToString> PartialEq for ApplyAction<T> {
    fn eq(&self, other: &ApplyAction<T>) -> bool {
        self.is_eq(other)
    }
}

macro_rules! impl_compare {
    () => {
            fn as_any(&self) -> &Any { self }
            fn is_eq(&self, other: &ApplyAction<T>) -> bool {
                if let Some(other) = other.as_any().downcast_ref::<Self>() {
                    self == other
                } else {
                    false
                }
            }
        }
}

/// Turn an edit script into a list of patches on the "src" and "dst" ASTs.
pub trait Patchify<T: Clone + fmt::Debug + ToString> {
    /// Turn object into a `Patch`. Non-terminal nodes are ignored.
    fn patchify(&self, store: &MappingStore<T>, src: &mut Vec<Patch>, dst: &mut Vec<Patch>);
    /// Turn object into a 'Patch'.
    fn patchify_chawathe98(&self,
                           store: &MappingStoreGraph<T>,
                           src: &mut Vec<Patch>,
                           dst: &mut Vec<Patch>);
}

/// A list of actions to be applied.
///
/// This type will usually be used by iterating over the list and calling
/// `apply()` on each element.
#[derive(Debug)]
pub struct EditScript<T: fmt::Debug + PartialEq> {
    actions: Vec<Box<ApplyAction<T>>>
}

/// Delete a node from a given AST.
///
/// It is only valid to delete leaf nodes.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Delete {
    node: NodeId<SrcNodeId>
}

/// Insert a new node into an AST as the `position`th child of an existing node.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Insert {
    /// New node in src-arena, must already exist before applying this trait.
    node: NodeId<SrcNodeId>,
    nth_child: u16,
    /// New parent in dst-arena.
    new_parent: Option<NodeId<SrcNodeId>>
}

/// Move a node from one place to another within an AST.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Move {
    src_node: NodeId<SrcNodeId>,
    parent: NodeId<SrcNodeId>,
    pos: u16
}

/// Update the data inside an AST node.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Update<T: Clone + fmt::Debug> {
    node: NodeId<SrcNodeId>,
    ty: T,
    label: String
}

/// Copy a subtree from one place to another with an AST.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Copy {
    src_node: NodeId<SrcNodeId>,
    parent: NodeId<SrcNodeId>,
    pos: u16
}

/// Glue a subtree src one place to another with an AST.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Glue {
    src_node: NodeId<SrcNodeId>,
    node: NodeId<SrcNodeId>,
    pos: u16
}

impl Delete {
    /// Create a new `Delete` object.
    pub fn new(node: NodeId<SrcNodeId>) -> Delete {
        Delete { node }
    }
}

impl Insert {
    /// Create a new `Insert` object.
    pub fn new(node: NodeId<SrcNodeId>,
               new_parent: Option<NodeId<SrcNodeId>>,
               nth_child: u16)
               -> Insert {
        Insert { node,
                 new_parent,
                 nth_child }
    }
}

impl Move {
    /// Create a new `Move` object.
    pub fn new(src_node: NodeId<SrcNodeId>, parent: NodeId<SrcNodeId>, pos: u16) -> Move {
        Move { src_node,
               parent,
               pos }
    }
}

impl<T: Clone + fmt::Debug> Update<T> {
    /// Create a new `Update` object.
    pub fn new(node: NodeId<SrcNodeId>, ty: T, label: String) -> Update<T> {
        Update { node, ty, label }
    }
}

impl Copy {
    /// Create a new `Copy` object.
    pub fn new(src_node: NodeId<SrcNodeId>, parent: NodeId<SrcNodeId>, pos: u16) -> Copy {
        Copy { src_node,
               parent,
               pos }
    }
}

impl Glue {
    /// Create a new `Glue` object.
    pub fn new(src: NodeId<SrcNodeId>, parent: NodeId<SrcNodeId>, position: u16) -> Glue {
        Glue { src_node: src,
               node: parent,
               pos: position }
    }
}

impl RenderJson for Delete {
    fn render_json(&self, indent: usize) -> String {
        let ind_s = " ".repeat(indent + 4);
        let mut json = vec![];
        json.push(" ".repeat(indent) + "{");
        json.push(format!("{}\"action\": \"delete\",", ind_s));
        json.push(format!("{}\"tree\": {}", &ind_s, self.node.id()));
        json.push(" ".repeat(indent) + "}");
        json.join("\n")
    }
}

impl RenderJson for Insert {
    fn render_json(&self, indent: usize) -> String {
        let ind_s = " ".repeat(indent + 4);
        let mut json = vec![];
        json.push(" ".repeat(indent) + "{");
        json.push(format!("{}\"action\": \"insert\",", ind_s));
        json.push(format!("{}\"tree\": {},", &ind_s, self.node.id()));
        match self.new_parent {
            Some(ref p) => json.push(format!("{}\"parent\": {},", &ind_s, p.id())),
            None => json.push(format!("{}\"parent\": {},", &ind_s, ""))
        }
        json.push(format!("{}\"at\": {}", &ind_s, self.nth_child));
        json.push(" ".repeat(indent) + "}");
        json.join("\n")
    }
}

impl RenderJson for Move {
    fn render_json(&self, indent: usize) -> String {
        let ind_s = " ".repeat(indent + 4);
        let mut json = vec![];
        json.push(" ".repeat(indent) + "{");
        json.push(format!("{}\"action\": \"move\",", ind_s));
        json.push(format!("{}\"tree\": {},", &ind_s, self.src_node.id()));
        json.push(format!("{}\"parent\": {},", &ind_s, self.parent));
        json.push(format!("{}\"at\": {}", &ind_s, self.pos));
        json.push(" ".repeat(indent) + "}");
        json.join("\n")
    }
}

impl<T: Clone + fmt::Debug> RenderJson for Update<T> {
    fn render_json(&self, indent: usize) -> String {
        let ind_s = " ".repeat(indent + 4);
        let mut json = vec![];
        json.push(" ".repeat(indent) + "{");
        json.push(format!("{}\"action\": \"update\",", ind_s));
        json.push(format!("{}\"tree\": {},", &ind_s, self.node.id()));
        json.push(format!("{}\"label\": \"{}\"", &ind_s, self.label));
        json.push(" ".repeat(indent) + "}");
        json.join("\n")
    }
}

impl RenderJson for Copy {
    fn render_json(&self, indent: usize) -> String {
        let ind_s = " ".repeat(indent + 4);
        let mut json = vec![];
        json.push(" ".repeat(indent) + "{");
        json.push(format!("{}\"action\": \"copy\",", ind_s));
        json.push(format!("{}\"tree\": {},", &ind_s, self.src_node.id()));
        json.push(format!("{}\"parent\": {},", &ind_s, self.parent));
        json.push(format!("{}\"at\": {}", &ind_s, self.pos));
        json.push(" ".repeat(indent) + "}");
        json.join("\n")
    }
}

impl RenderJson for Glue {
    fn render_json(&self, indent: usize) -> String {
        let ind_s = " ".repeat(indent + 4);
        let mut json = vec![];
        json.push(" ".repeat(indent) + "{");
        json.push(format!("{}\"action\": \"glue\",", ind_s));
        json.push(format!("{}\"tree\": {},", &ind_s, self.src_node.id()));
        json.push(format!("{}\"src\": {},", &ind_s, self.node));
        json.push(format!("{}\"at\": {}", &ind_s, self.pos));
        json.push(" ".repeat(indent) + "}");
        json.join("\n")
    }
}

impl<T: Clone + fmt::Debug + Eq + ToString> Patchify<T> for Delete {
    fn patchify(&self, store: &MappingStore<T>, src: &mut Vec<Patch>, _: &mut Vec<Patch>) {
        let node = &store.src_arena.borrow()[self.node];
        if node.char_no.is_some() {
            src.push(Patch::new(ActionType::DELETE,
                                node.char_no.unwrap(),
                                node.token_len.unwrap()));
        }
    }
    fn patchify_chawathe98(&self,
                           store: &MappingStoreGraph<T>,
                           src: &mut Vec<Patch>,
                           _: &mut Vec<Patch>) {
        let node = &store.src_arena.borrow()[self.node];
        if node.char_no.is_some() {
            src.push(Patch::new(ActionType::DELETE,
                                node.char_no.unwrap(),
                                node.token_len.unwrap()));
        }
    }
}

impl<T: Clone + fmt::Debug + Eq + ToString> Patchify<T> for Insert {
    fn patchify(&self, store: &MappingStore<T>, _: &mut Vec<Patch>, dst: &mut Vec<Patch>) {
        let node = &store.src_arena.borrow()[self.node];
        if node.char_no.is_some() {
            dst.push(Patch::new(ActionType::INSERT,
                                node.char_no.unwrap(),
                                node.token_len.unwrap()));
        }
    }

    fn patchify_chawathe98(&self,
                           store: &MappingStoreGraph<T>,
                           _: &mut Vec<Patch>,
                           dst: &mut Vec<Patch>) {
        let node = &store.src_arena.borrow()[self.node];
        if node.char_no.is_some() {
            dst.push(Patch::new(ActionType::INSERT,
                                node.char_no.unwrap(),
                                node.token_len.unwrap()));
        }
    }
}

impl<T: Clone + fmt::Debug + Eq + ToString + 'static> Patchify<T> for Move {
    fn patchify(&self, store: &MappingStore<T>, src: &mut Vec<Patch>, dst: &mut Vec<Patch>) {
        let f_node = &store.src_arena.borrow()[self.src_node];
        if f_node.char_no.is_some() {
            src.push(Patch::new(ActionType::MOVE,
                                f_node.char_no.unwrap(),
                                f_node.token_len.unwrap()));
        }
        let t_node = &store.dst_arena.borrow()[store.get_dst(self.src_node).unwrap()];
        if t_node.char_no.is_some() {
            dst.push(Patch::new(ActionType::MOVE,
                                t_node.char_no.unwrap(),
                                t_node.token_len.unwrap()));
        }
    }

    fn patchify_chawathe98(&self,
                           store: &MappingStoreGraph<T>,
                           src: &mut Vec<Patch>,
                           dst: &mut Vec<Patch>) {
        let f_node = &store.src_arena.borrow()[self.src_node];
        if f_node.char_no.is_some() {
            src.push(Patch::new(ActionType::MOVE,
                                f_node.char_no.unwrap(),
                                f_node.token_len.unwrap()));
        }
        let t_node = &store.dst_arena.borrow()[store.get_dst(self.src_node).unwrap()];
        if t_node.char_no.is_some() {
            dst.push(Patch::new(ActionType::MOVE,
                                t_node.char_no.unwrap(),
                                t_node.token_len.unwrap()));
        }
    }
}

impl<T: Clone + fmt::Debug + Eq + ToString + 'static> Patchify<T> for Update<T> {
    fn patchify(&self, store: &MappingStore<T>, src: &mut Vec<Patch>, dst: &mut Vec<Patch>) {
        let f_node = &store.src_arena.borrow()[self.node];
        if f_node.char_no.is_some() {
            src.push(Patch::new(ActionType::UPDATE,
                                f_node.char_no.unwrap(),
                                f_node.token_len.unwrap()));
        }
        let t_node = &store.dst_arena.borrow()[store.get_dst(self.node).unwrap()];
        if t_node.char_no.is_some() {
            dst.push(Patch::new(ActionType::UPDATE,
                                t_node.char_no.unwrap(),
                                t_node.token_len.unwrap()));
        }
    }

    fn patchify_chawathe98(&self,
                           store: &MappingStoreGraph<T>,
                           src: &mut Vec<Patch>,
                           dst: &mut Vec<Patch>) {
        let f_node = &store.src_arena.borrow()[self.node];
        if f_node.char_no.is_some() {
            src.push(Patch::new(ActionType::UPDATE,
                                f_node.char_no.unwrap(),
                                f_node.token_len.unwrap()));
        }
        let t_node = &store.dst_arena.borrow()[store.get_dst(self.node).unwrap()];
        if t_node.char_no.is_some() {
            dst.push(Patch::new(ActionType::UPDATE,
                                t_node.char_no.unwrap(),
                                t_node.token_len.unwrap()));
        }
    }
}

impl<T: Clone + fmt::Debug + Eq + ToString + 'static> Patchify<T> for Copy {
    fn patchify(&self, store: &MappingStore<T>, src: &mut Vec<Patch>, dst: &mut Vec<Patch>) {
        let f_node = &store.src_arena.borrow()[self.src_node];
        if f_node.char_no.is_some() {
            src.push(Patch::new(ActionType::COPY,
                                f_node.char_no.unwrap(),
                                f_node.token_len.unwrap()));
        }
        let t_node = &store.dst_arena.borrow()[store.get_dst(self.src_node).unwrap()];
        if t_node.char_no.is_some() {
            dst.push(Patch::new(ActionType::COPY,
                                t_node.char_no.unwrap(),
                                t_node.token_len.unwrap()));
        }
    }

    fn patchify_chawathe98(&self,
                           store: &MappingStoreGraph<T>,
                           src: &mut Vec<Patch>,
                           dst: &mut Vec<Patch>) {
        let f_node = &store.src_arena.borrow()[self.src_node];
        if f_node.char_no.is_some() {
            src.push(Patch::new(ActionType::COPY,
                                f_node.char_no.unwrap(),
                                f_node.token_len.unwrap()));
        }
        let t_node = &store.dst_arena.borrow()[store.get_dst(self.src_node).unwrap()];
        if t_node.char_no.is_some() {
            dst.push(Patch::new(ActionType::COPY,
                                t_node.char_no.unwrap(),
                                t_node.token_len.unwrap()));
        }
    }
}

impl<T: Clone + fmt::Debug + Eq + ToString + 'static> Patchify<T> for Glue {
    fn patchify(&self, store: &MappingStore<T>, src: &mut Vec<Patch>, dst: &mut Vec<Patch>) {
        let f_node = &store.src_arena.borrow()[self.src_node];
        if f_node.char_no.is_some() {
            src.push(Patch::new(ActionType::GLUE,
                                f_node.char_no.unwrap(),
                                f_node.token_len.unwrap()));
        }
        let t_node = &store.dst_arena.borrow()[store.get_dst(self.src_node).unwrap()];
        if t_node.char_no.is_some() {
            dst.push(Patch::new(ActionType::GLUE,
                                t_node.char_no.unwrap(),
                                t_node.token_len.unwrap()));
        }
    }

    fn patchify_chawathe98(&self,
                           store: &MappingStoreGraph<T>,
                           src: &mut Vec<Patch>,
                           dst: &mut Vec<Patch>) {
        let f_node = &store.src_arena.borrow()[self.src_node];
        if f_node.char_no.is_some() {
            src.push(Patch::new(ActionType::GLUE,
                                f_node.char_no.unwrap(),
                                f_node.token_len.unwrap()));
        }
        let t_node = &store.dst_arena.borrow()[store.get_dst(self.src_node).unwrap()];
        if t_node.char_no.is_some() {
            dst.push(Patch::new(ActionType::GLUE,
                                t_node.char_no.unwrap(),
                                t_node.token_len.unwrap()));
        }
    }
}

impl<T: Clone + fmt::Debug + Eq + PartialEq + ToString> ApplyAction<T> for Delete {
    fn apply(&mut self, arena: &mut Arena<T, SrcNodeId>) -> ArenaResult {
        debug_assert!(self.node.is_leaf(arena),
                      "Attempt to delete branch node {}",
                      self.node);
        self.node.detach(arena)
    }
    impl_compare!();
}

impl<T: Clone + fmt::Debug + Eq + PartialEq + ToString> ApplyAction<T> for Insert {
    fn apply(&mut self, arena: &mut Arena<T, SrcNodeId>) -> ArenaResult {
        match self.new_parent {
            Some(p) => self.node.make_nth_child_of(p, self.nth_child, arena),
            None => arena.new_root(self.node)
        }
    }
    impl_compare!();
}

impl<T: Clone + fmt::Debug + Eq + PartialEq + ToString + 'static> ApplyAction<T> for Move {
    fn apply(&mut self, arena: &mut Arena<T, SrcNodeId>) -> ArenaResult {
        self.src_node
            .make_nth_child_of(self.parent, self.pos, arena)
    }
    impl_compare!();
}

impl<T: Clone + fmt::Debug + Eq + PartialEq + ToString + 'static> ApplyAction<T> for Update<T> {
    fn apply(&mut self, arena: &mut Arena<T, SrcNodeId>) -> ArenaResult {
        if !arena.contains(self.node) {
            return Err(ArenaError::NodeIdNotFound(self.node.id()));
        }
        arena[self.node].ty = self.ty.clone();
        arena[self.node].label = self.label.clone();
        Ok(())
    }
    impl_compare!();
}

impl<T: Clone + fmt::Debug + Eq + PartialEq + ToString + 'static> ApplyAction<T> for Copy {
    fn apply(&mut self, arena: &mut Arena<T, SrcNodeId>) -> ArenaResult {
        if !arena.contains(self.src_node) {
            return Err(ArenaError::NodeIdNotFound(self.src_node.id()));
        }
        self.src_node.copy_subtree(self.parent, self.pos, arena)
    }
    impl_compare!();
}

impl<T: Clone + fmt::Debug + Eq + PartialEq + ToString + 'static> ApplyAction<T> for Glue {
    fn apply(&mut self, arena: &mut Arena<T, SrcNodeId>) -> ArenaResult {
        self.src_node.make_nth_child_of(self.node, self.pos, arena)
    }
    impl_compare!();
}

impl<T: Clone + fmt::Debug + PartialEq> Default for EditScript<T> {
    fn default() -> EditScript<T> {
        EditScript { actions: vec![] }
    }
}

impl<T: Clone + fmt::Debug + Eq + PartialEq + ToString> EditScript<T> {
    /// Create an empty list of actions.
    pub fn new() -> EditScript<T> {
        Default::default()
    }

    /// Push an action onto the action list.
    pub fn push<A: ApplyAction<T> + 'static>(&mut self, action: A) {
        self.actions.push(Box::new(action));
    }

    /// Reverse the actions in an edit script.
    pub fn reverse(&mut self) {
        self.actions.reverse();
    }

    /// Remove all actions from this edit script.
    pub fn clear(&mut self) {
        self.actions.clear();
    }

    /// Test whether this edit script is empty.
    pub fn is_empty(&self) -> bool {
        self.actions.is_empty()
    }

    /// Return the number of actions in this list.
    pub fn size(&self) -> usize {
        self.actions.len()
    }
}

impl<T: Clone + fmt::Debug + PartialEq> RenderJson for EditScript<T> {
    fn render_json(&self, indent: usize) -> String {
        let mut json = vec![];
        for action in &self.actions {
            json.push(action.render_json(indent + 4));
        }
        format!("{}{}{}{}{}{}",
                " ".repeat(indent),
                "\"actions\": [\n",
                json.join(",\n"),
                "\n",
                " ".repeat(indent),
                "]")
    }
}

impl<T: Clone + fmt::Debug + Eq + ToString> Patchify<T> for EditScript<T> {
    fn patchify(&self, store: &MappingStore<T>, src: &mut Vec<Patch>, dst: &mut Vec<Patch>) {
        for action in &self.actions {
            action.patchify(store, src, dst);
        }
    }

    fn patchify_chawathe98(&self,
                           store: &MappingStoreGraph<T>,
                           src: &mut Vec<Patch>,
                           dst: &mut Vec<Patch>) {
        for action in &self.actions {
            action.patchify_chawathe98(store, src, dst);
        }
    }
}

impl<T: Clone + fmt::Debug + Eq + PartialEq + ToString + 'static> PartialEq for EditScript<T> {
    fn eq(&self, other: &EditScript<T>) -> bool {
        if self.actions.len() != other.actions.len() {
            return false;
        }
        for (i, action) in self.actions.iter().enumerate() {
            if **action != *other.actions[i] {
                return false;
            }
        }
        true
    }
}

impl<T: Clone + fmt::Debug + Eq + PartialEq + ToString + 'static> ApplyAction<T> for EditScript<T> {
    fn apply(&mut self, arena: &mut Arena<T, SrcNodeId>) -> ArenaResult {
        for boxed_action in &mut self.actions {
            boxed_action.apply(arena)?;
        }
        Ok(())
    }
    impl_compare!();
}

#[cfg(test)]
mod test {
    use super::*;
    use test_common::create_mult_arena;

    #[test]
    fn apply_delete_leaf() {
        let mut arena = create_mult_arena();
        let format1 = "000: \"Expr\" +
001:   \"INT\" 1
002:   \"Expr\" *
003:     \"INT\" 3
004:     \"INT\" 4
";

        assert_eq!(format1, format!("{:?}", arena));
        let mut del1 = Delete::new(NodeId::new(3));
        del1.apply(&mut arena).unwrap();
        let mut del2 = Delete::new(NodeId::new(4));
        del2.apply(&mut arena).unwrap();
        let format2 = "000: \"Expr\" +
001:   \"INT\" 1
002:   \"Expr\" *
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    #[should_panic]
    fn apply_delete_branch() {
        let mut arena = create_mult_arena();
        let format1 = "000: \"Expr\" +
001:   \"INT\" 1
002:   \"Expr\" *
003:     \"INT\" 3
004:     \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        let mut del = Delete::new(NodeId::new(2));
        del.apply(&mut arena).unwrap();
    }

    #[test]
    fn apply_insert() {
        let mut arena = create_mult_arena();
        let n5 = arena.new_node("INT".to_string(), "100".to_string(), None, None, None, None);
        let format1 = "000: \"Expr\" +
001:   \"INT\" 1
002:   \"Expr\" *
003:     \"INT\" 3
004:     \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        let mut ins = Insert::new(n5, Some(NodeId::new(2)), 0);
        ins.apply(&mut arena).unwrap();
        let format2 = "000: \"Expr\" +
001:   \"INT\" 1
002:   \"Expr\" *
005:     \"INT\" 100
003:     \"INT\" 3
004:     \"INT\" 4
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    fn apply_insert_new_root() {
        let mut arena = create_mult_arena();
        let n5 = arena.new_node("Expr".to_string(), "-".to_string(), None, None, None, None);
        let format1 = "000: \"Expr\" +
001:   \"INT\" 1
002:   \"Expr\" *
003:     \"INT\" 3
004:     \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        let mut ins = Insert::new(n5, None, 0);
        ins.apply(&mut arena).unwrap();
        let format2 = "005: \"Expr\" -
000:   \"Expr\" +
001:     \"INT\" 1
002:     \"Expr\" *
003:       \"INT\" 3
004:       \"INT\" 4
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    fn apply_move() {
        let mut arena = create_mult_arena();
        let format1 = "000: \"Expr\" +
001:   \"INT\" 1
002:   \"Expr\" *
003:     \"INT\" 3
004:     \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        let mut mov = Move::new(NodeId::new(4), NodeId::new(2), 0);
        mov.apply(&mut arena).unwrap();
        let format2 = "000: \"Expr\" +
001:   \"INT\" 1
002:   \"Expr\" *
004:     \"INT\" 4
003:     \"INT\" 3
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    fn apply_update() {
        let mut arena = create_mult_arena();
        assert_eq!(5, arena.size());
        let format1 = "000: \"Expr\" +
001:   \"INT\" 1
002:   \"Expr\" *
003:     \"INT\" 3
004:     \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        let mut upd = Update::new(NodeId::new(2), "Expr".to_string(), "+".to_string());
        upd.apply(&mut arena).unwrap();
        let format2 = "000: \"Expr\" +
001:   \"INT\" 1
002:   \"Expr\" +
003:     \"INT\" 3
004:     \"INT\" 4
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    fn apply_to_list1() {
        let mut arena = create_mult_arena();
        let format1 = "000: \"Expr\" +
001:   \"INT\" 1
002:   \"Expr\" *
003:     \"INT\" 3
004:     \"INT\" 4
";
        let n5 = arena.new_node("INT".to_string(), "100".to_string(), None, None, None, None);
        let n6 = arena.new_node("INT".to_string(), "99".to_string(), None, None, None, None);
        assert_eq!(format1, format!("{:?}", arena));
        // Create action list.
        let mut actions: EditScript<String> = EditScript::new();
        assert!(actions.is_empty());
        let del1 = Delete::new(NodeId::new(3)); // INT 3
        let del2 = Delete::new(NodeId::new(4)); // INT 4
        let ins1 = Insert::new(n5, Some(NodeId::new(2)), 0);
        let ins2 = Insert::new(n6, Some(NodeId::new(2)), 1);
        let mov = Move::new(NodeId::new(6), NodeId::new(2), 0); // Swap "INT 100" and "INT 99".
                                                                // Change "+"" to "*".
        let upd = Update::new(NodeId::new(0), "Expr".to_string(), "*".to_string());
        actions.push(del1);
        actions.push(del2);
        actions.push(ins1);
        actions.push(ins2);
        actions.push(mov);
        actions.push(upd);
        assert!(!actions.is_empty());
        assert_eq!(6, actions.size());
        // Apply action list.
        actions.apply(&mut arena).unwrap();
        let format2 = "000: \"Expr\" *
001:   \"INT\" 1
002:   \"Expr\" *
006:     \"INT\" 99
005:     \"INT\" 100
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    fn apply_to_list2() {
        let mut arena = create_mult_arena();
        let format1 = "000: \"Expr\" +
001:   \"INT\" 1
002:   \"Expr\" *
003:     \"INT\" 3
004:     \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        let n5 = arena.new_node("INT".to_string(), "2".to_string(), None, None, None, None);
        // Create action list.
        let mut actions: EditScript<String> = EditScript::new();
        assert!(actions.is_empty());
        let del = Delete { node: NodeId::new(4) }; // Remove "4".
        let ins = Insert { node: n5,
                           new_parent: Some(NodeId::new(2)),
                           nth_child: 0 };
        let upd = Update { // Change "+" to "*".
                           node: NodeId::new(0),
                           ty: "Expr".to_string(),
                           label: "*".to_string() };
        actions.push(del);
        actions.push(ins);
        actions.push(upd);
        assert!(!actions.is_empty());
        assert_eq!(3, actions.size());
        // Apply action list.
        actions.apply(&mut arena).unwrap();
        let format2 = "000: \"Expr\" *
001:   \"INT\" 1
002:   \"Expr\" *
005:     \"INT\" 2
003:     \"INT\" 3
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    fn apply_partial_eq() {
        let mut arena1 = create_mult_arena();
        let n5 = arena1.new_node("INT".to_string(), "100".to_string(), None, None, None, None);
        let n6 = arena1.new_node("INT".to_string(), "99".to_string(), None, None, None, None);
        // Create action list.
        let mut actions1: EditScript<String> = EditScript::new();
        assert!(actions1.is_empty());
        let del1 = Delete::new(NodeId::new(3)); // INT 3
        let del2 = Delete::new(NodeId::new(4)); // INT 4
        assert_eq!(del1, del1);
        assert_eq!(del2, del2);
        assert_ne!(del1, del2);
        let ins1 = Insert::new(n5, Some(NodeId::new(2)), 0);
        let ins2 = Insert::new(n6, Some(NodeId::new(2)), 1);
        assert_eq!(ins1, ins1);
        assert_eq!(ins2, ins2);
        assert_ne!(ins1, ins2);
        let mov1 = Move::new(NodeId::new(6), NodeId::new(2), 0); // Swap "INT 100" and "INT 99".
        let upd1 = Update::new(NodeId::new(0), "Expr".to_string(), "*".to_string());
        actions1.push(del1);
        actions1.push(del2);
        actions1.push(ins1);
        actions1.push(ins2);
        actions1.push(mov1);
        actions1.push(upd1);
        assert!(!actions1.is_empty());
        assert_eq!(6, actions1.size());
        // Second edit script.
        let mut arena2 = create_mult_arena();
        let n7 = arena2.new_node("INT".to_string(), "2".to_string(), None, None, None, None);
        let mut actions2: EditScript<String> = EditScript::new();
        assert!(actions2.is_empty());
        let del3 = Delete { node: NodeId::new(4) }; // Remove "4".
        let ins3 = Insert { node: n7,
                            new_parent: Some(NodeId::new(2)),
                            nth_child: 0 };
        let upd2 = Update { // Change "+" to "*".
                            node: NodeId::new(0),
                            ty: "Expr".to_string(),
                            label: "*".to_string() };
        actions2.push(del3);
        actions2.push(ins3);
        actions2.push(upd2);
        assert!(!actions2.is_empty());
        assert_eq!(3, actions2.size());
        assert_eq!(actions1, actions1);
        assert_eq!(actions2, actions2);
        assert_ne!(actions1, actions2);
    }

    #[test]
    fn render_json() {
        let mut actions: EditScript<&str> = EditScript::new();
        let del1 = Delete::new(NodeId::new(3)); // INT 3
        let del2 = Delete::new(NodeId::new(4)); // INT 4
        let ins1 = Insert::new(NodeId::new(5), Some(NodeId::new(2)), 0);
        let ins2 = Insert::new(NodeId::new(6), Some(NodeId::new(2)), 1);
        let mov = Move::new(NodeId::new(6), NodeId::new(2), 0); // Swap "INT 100" and "INT 99".
                                                                // Change "+"" to "*".
        let upd = Update::new(NodeId::new(0), "Expr", String::from("*"));
        actions.push(del1);
        actions.push(del2);
        actions.push(ins1);
        actions.push(ins2);
        actions.push(mov);
        actions.push(upd);
        assert_eq!(6, actions.size());
        let expected = String::from(
                                    "\"actions\": [
    {
        \"action\": \"delete\",
        \"tree\": 3
    },
    {
        \"action\": \"delete\",
        \"tree\": 4
    },
    {
        \"action\": \"insert\",
        \"tree\": 5,
        \"parent\": 2,
        \"at\": 0
    },
    {
        \"action\": \"insert\",
        \"tree\": 6,
        \"parent\": 2,
        \"at\": 1
    },
    {
        \"action\": \"move\",
        \"tree\": 6,
        \"parent\": 2,
        \"at\": 0
    },
    {
        \"action\": \"update\",
        \"tree\": 0,
        \"label\": \"*\"
    }
]"
        );
        assert_eq!(expected, actions.render_json(0));
    }
}

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

use std::boxed::Box;
use std::clone::Clone;
use std::fmt;

use ast::{Arena, ArenaError, ArenaResult, FromNodeId, NodeId};
use emitters::RenderJson;
use matchers::MappingStore;
use patch::Patch;

/// Type of action.
///
/// Only used where the information related to actions can safely be discarded.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ActionType {
    /// Delete a node in a 'from' arena.
    DELETE,
    /// Insert a new node into the 'from' arena.
    INSERT,
    /// Move an existing node.
    MOVE,
    /// Update the text or label associated with a node.
    UPDATE,
    /// Copy takes the node or subtree 'from tree' and and copies it into 'to tree'.
    COPY,
    /// GLUE takes a subtree/node 'from tree' and inserts it into 'to tree'
    GLUE,
}

/// Apply an action to an AST node.
pub trait ApplyAction<T: Clone + fmt::Debug>: fmt::Debug + Patchify<T> + RenderJson {
    /// Apply an action to an AST.
    fn apply(&mut self, arena: &mut Arena<T, FromNodeId>) -> ArenaResult;
}

/// Turn an edit script into a list of patches on the "from" and "to" ASTs.
pub trait Patchify<T: Clone + fmt::Debug> {
    /// Turn object into a `Patch`. Non-terminal nodes are ignored.
    fn patchify(&self, store: &MappingStore<T>, from: &mut Vec<Patch>, to: &mut Vec<Patch>);
}

/// A list of actions to be applied.
///
/// This type will usually be used by iterating over the list and calling
/// `apply()` on each element.
pub struct EditScript<T: fmt::Debug> {
    actions: Vec<Box<ApplyAction<T>>>,
}

/// Delete a node from a given AST.
///
/// It is only valid to delete leaf nodes.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Delete {
    node: NodeId<FromNodeId>,
}

/// Insert a new node into an AST as the `position`th child of an existing node.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Insert {
    /// New node in from-arena, must already exist before applying this trait.
    node: NodeId<FromNodeId>,
    nth_child: u16,
    /// New parent in to-arena.
    new_parent: Option<NodeId<FromNodeId>>,
}

/// Move a node from one place to another within an AST.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Move {
    from_node: NodeId<FromNodeId>,
    parent: NodeId<FromNodeId>,
    pos: u16,
}

/// Update the data inside an AST node.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Update<T: Clone + fmt::Debug> {
    node: NodeId<FromNodeId>,
    ty: T,
    label: String,
}

/// Copy a subtree from one place to another with an AST.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Copy {
    from_node: NodeId<FromNodeId>,
    parent: NodeId<FromNodeId>,
    pos: u16,
}

/// Glue a subtree from one place to another with an AST.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Glue {
    from_node: NodeId<FromNodeId>,
    node: NodeId<FromNodeId>,
}

impl Delete {
    /// Create a new `Delete` object.
    pub fn new(node: NodeId<FromNodeId>) -> Delete {
        Delete { node: node }
    }
}

impl Insert {
    /// Create a new `Insert` object.
    pub fn new(node: NodeId<FromNodeId>, parent: Option<NodeId<FromNodeId>>, nth: u16) -> Insert {
        Insert { node: node,
                 new_parent: parent,
                 nth_child: nth, }
    }
}

impl Move {
    /// Create a new `Move` object.
    pub fn new(from: NodeId<FromNodeId>, parent: NodeId<FromNodeId>, position: u16) -> Move {
        Move { from_node: from,
               parent: parent,
               pos: position, }
    }
}

impl<T: Clone + fmt::Debug> Update<T> {
    /// Create a new `Update` object.
    pub fn new(node: NodeId<FromNodeId>, ty: T, label: String) -> Update<T> {
        Update { node: node,
                 ty: ty,
                 label: label, }
    }
}

impl Copy {
    /// Create a new `Copy` object.
    pub fn new(from: NodeId<FromNodeId>, parent: NodeId<FromNodeId>, position: u16) -> Copy {
        Copy { from_node: from,
               parent: parent,
               pos: position, }
    }
}

impl Glue {
    /// Create a new `Glue` object.
    pub fn new(from: NodeId<FromNodeId>, parent: NodeId<FromNodeId>) -> Glue {
        Glue { from_node: from,
               node: parent, }
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
            None => json.push(format!("{}\"parent\": {},", &ind_s, "")),
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
        json.push(format!("{}\"tree\": {},", &ind_s, self.from_node.id()));
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
        json.push(format!("{}\"tree\": {},", &ind_s, self.from_node.id()));
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
        json.push(format!("{}\"tree\": {},", &ind_s, self.from_node.id()));
        json.push(format!("{}\"from\": {},", &ind_s, self.node));
        json.push(" ".repeat(indent) + "}");
        json.join("\n")
    }
}

impl<T: Clone + fmt::Debug + Eq> Patchify<T> for Delete {
    fn patchify(&self, store: &MappingStore<T>, from: &mut Vec<Patch>, _: &mut Vec<Patch>) {
        let node = &store.from_arena.borrow()[self.node];
        if node.char_no.is_some() {
            from.push(Patch::new(ActionType::DELETE,
                                 node.char_no.unwrap(),
                                 node.token_len.unwrap()));
        }
    }
}

impl<T: Clone + fmt::Debug + Eq> Patchify<T> for Insert {
    fn patchify(&self, store: &MappingStore<T>, _: &mut Vec<Patch>, to: &mut Vec<Patch>) {
        let node = &store.from_arena.borrow()[self.node];
        if node.char_no.is_some() {
            to.push(Patch::new(ActionType::INSERT,
                               node.char_no.unwrap(),
                               node.token_len.unwrap()));
        }
    }
}

impl<T: Clone + fmt::Debug + Eq + 'static> Patchify<T> for Move {
    fn patchify(&self, store: &MappingStore<T>, from: &mut Vec<Patch>, to: &mut Vec<Patch>) {
        let f_node = &store.from_arena.borrow()[self.from_node];
        if f_node.char_no.is_some() {
            from.push(Patch::new(ActionType::MOVE,
                                 f_node.char_no.unwrap(),
                                 f_node.token_len.unwrap()));
        }
        let t_node = &store.to_arena.borrow()[store.get_to(&self.from_node).unwrap()];
        if t_node.char_no.is_some() {
            to.push(Patch::new(ActionType::MOVE,
                               t_node.char_no.unwrap(),
                               t_node.token_len.unwrap()));
        }
    }
}

impl<T: Clone + fmt::Debug + Eq + 'static> Patchify<T> for Update<T> {
    fn patchify(&self, store: &MappingStore<T>, from: &mut Vec<Patch>, to: &mut Vec<Patch>) {
        let f_node = &store.from_arena.borrow()[self.node];
        if f_node.char_no.is_some() {
            from.push(Patch::new(ActionType::UPDATE,
                                 f_node.char_no.unwrap(),
                                 f_node.token_len.unwrap()));
        }
        let t_node = &store.to_arena.borrow()[store.get_to(&self.node).unwrap()];
        if t_node.char_no.is_some() {
            to.push(Patch::new(ActionType::UPDATE,
                               t_node.char_no.unwrap(),
                               t_node.token_len.unwrap()));
        }
    }
}

impl<T: Clone + fmt::Debug + Eq + 'static> Patchify<T> for Copy {
    fn patchify(&self, store: &MappingStore<T>, from: &mut Vec<Patch>, to: &mut Vec<Patch>) {
        let f_node = &store.from_arena.borrow()[self.from_node];
        if f_node.char_no.is_some() {
            from.push(Patch::new(ActionType::COPY,
                                 f_node.char_no.unwrap(),
                                 f_node.token_len.unwrap()));
        }
        let t_node = &store.to_arena.borrow()[store.get_to(&self.from_node).unwrap()];
        if t_node.char_no.is_some() {
            to.push(Patch::new(ActionType::COPY,
                               t_node.char_no.unwrap(),
                               t_node.token_len.unwrap()));
        }
    }
}

impl<T: Clone + fmt::Debug + Eq + 'static> Patchify<T> for Glue{
    fn patchify(&self, store: &MappingStore<T>, from: &mut Vec<Patch>, to: &mut Vec<Patch>) {
        let f_node = &store.from_arena.borrow()[self.from_node];
        if f_node.char_no.is_some() {
            from.push(Patch::new(ActionType::GLUE,
                                 f_node.char_no.unwrap(),
                                 f_node.token_len.unwrap()));
        }
        let t_node = &store.to_arena.borrow()[store.get_to(&self.from_node).unwrap()];
        if t_node.char_no.is_some() {
            to.push(Patch::new(ActionType::GLUE,
                               t_node.char_no.unwrap(),
                               t_node.token_len.unwrap()));
        }
    }
}

impl<T: Clone + fmt::Debug + Eq> ApplyAction<T> for Delete {
    fn apply(&mut self, arena: &mut Arena<T, FromNodeId>) -> ArenaResult {
        debug_assert!(self.node.is_leaf(arena),
                      "Attempt to delete branch node {}",
                      self.node);
        self.node.detach(arena)
    }
}

impl<T: Clone + fmt::Debug + Eq> ApplyAction<T> for Insert {
    fn apply(&mut self, arena: &mut Arena<T, FromNodeId>) -> ArenaResult {
        match self.new_parent {
            Some(p) => self.node.make_nth_child_of(p, self.nth_child, arena),
            None => arena.new_root(self.node),
        }
    }
}

impl<T: Clone + fmt::Debug + Eq + 'static> ApplyAction<T> for Move {
    fn apply(&mut self, arena: &mut Arena<T, FromNodeId>) -> ArenaResult {
        self.from_node.make_nth_child_of(self.parent, self.pos, arena)
    }
}

impl<T: Clone + fmt::Debug + Eq + 'static> ApplyAction<T> for Update<T> {
    fn apply(&mut self, arena: &mut Arena<T, FromNodeId>) -> ArenaResult {
        if !arena.contains(self.node) {
            return Err(ArenaError::NodeIdNotFound);
        }
        arena[self.node].ty = self.ty.clone();
        arena[self.node].label = self.label.clone();
        Ok(())
    }
}

impl<T: Clone + fmt::Debug + Eq + 'static> ApplyAction<T> for Copy {
    fn apply(&mut self, arena: &mut Arena<T, FromNodeId>) -> ArenaResult {
        if !arena.contains(self.from_node) {
            return Err(ArenaError::NodeIdNotFound);
        }
        self.from_node.copy_subtree(self.parent, self.pos, arena)
    }
}

impl<T: Clone + fmt::Debug + Eq + 'static> ApplyAction<T> for Glue {
    fn apply(&mut self, arena: &mut Arena<T, FromNodeId>) -> ArenaResult {
        self.from_node.detach_with_children(arena)
    }
}

impl<T: Clone + fmt::Debug> Default for EditScript<T> {
    fn default() -> EditScript<T> {
        EditScript { actions: vec![] }
    }
}

impl<T: Clone + fmt::Debug + Eq> EditScript<T> {
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

    /// Return the number of actions in this list.
    pub fn size(&self) -> usize {
        self.actions.len()
    }
}

impl<T: Clone + fmt::Debug> RenderJson for EditScript<T> {
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

impl<T: Clone + fmt::Debug + Eq> Patchify<T> for EditScript<T> {
    fn patchify(&self, store: &MappingStore<T>, from: &mut Vec<Patch>, to: &mut Vec<Patch>) {
        for action in &self.actions {
            action.patchify(store, from, to);
        }
    }
}

impl<T: Clone + fmt::Debug + Eq> ApplyAction<T> for EditScript<T> {
    fn apply(&mut self, arena: &mut Arena<T, FromNodeId>) -> ArenaResult {
        for boxed_action in &mut self.actions {
            boxed_action.apply(arena)?;
        }
        Ok(())
    }
}

impl<T: fmt::Debug + Clone> fmt::Debug for EditScript<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "EditScript {{\n")?;
        for action in self.actions.iter() {
            write!(f, "    {:?}\n", action)?;
        }
        write!(f, "}}\n")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn create_arena() -> Arena<&'static str, FromNodeId> {
        let mut arena = Arena::new();
        let root = arena.new_node("Expr", String::from("+"), None, None, None, None);
        let n1 = arena.new_node("INT", String::from("1"), None, None, None, None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node("Expr", String::from("*"), None, None, None, None);
        n2.make_child_of(root, &mut arena).unwrap();
        let n3 = arena.new_node("INT", String::from("3"), None, None, None, None);
        n3.make_child_of(n2, &mut arena).unwrap();
        let n4 = arena.new_node("INT", String::from("4"), None, None, None, None);
        n4.make_child_of(n2, &mut arena).unwrap();
        arena
    }

    #[test]
    fn apply_delete_leaf() {
        let mut arena = create_arena();
        let format1 = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 3
    \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        let mut del1 = Delete::new(NodeId::new(3));
        del1.apply(&mut arena).unwrap();
        let mut del2 = Delete::new(NodeId::new(4));
        del2.apply(&mut arena).unwrap();
        let format2 = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    #[should_panic]
    fn apply_delete_branch() {
        let mut arena = create_arena();
        let format1 = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 3
    \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        let mut del = Delete::new(NodeId::new(2));
        del.apply(&mut arena).unwrap();
    }

    #[test]
    fn apply_insert() {
        let mut arena = create_arena();
        let n5 = arena.new_node("INT", String::from("100"), None, None, None, None);
        let format1 = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 3
    \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        let mut ins = Insert::new(n5, Some(NodeId::new(2)), 0);
        ins.apply(&mut arena).unwrap();
        let format2 = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 100
    \"INT\" 3
    \"INT\" 4
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    fn apply_insert_new_root() {
        let mut arena = create_arena();
        let n5 = arena.new_node("Expr", String::from("-"), None, None, None, None);
        let format1 = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 3
    \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        let mut ins = Insert::new(n5, None, 0);
        ins.apply(&mut arena).unwrap();
        let format2 = "\"Expr\" -
  \"Expr\" +
    \"INT\" 1
    \"Expr\" *
      \"INT\" 3
      \"INT\" 4
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    fn apply_move() {
        let mut arena = create_arena();
        let format1 = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 3
    \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        let mut mov = Move::new(NodeId::new(4), NodeId::new(2), 0);
        mov.apply(&mut arena).unwrap();
        let format2 = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 4
    \"INT\" 3
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    fn apply_update() {
        let mut arena = create_arena();
        assert_eq!(5, arena.size());
        let format1 = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 3
    \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        let mut upd = Update::new(NodeId::new(2), "Expr", String::from("+"));
        upd.apply(&mut arena).unwrap();
        let format2 = "\"Expr\" +
  \"INT\" 1
  \"Expr\" +
    \"INT\" 3
    \"INT\" 4
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    fn apply_to_list1() {
        let mut arena = create_arena();
        let format1 = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 3
    \"INT\" 4
";
        let n5 = arena.new_node("INT", String::from("100"), None, None, None, None);
        let n6 = arena.new_node("INT", String::from("99"), None, None, None, None);
        assert_eq!(format1, format!("{:?}", arena));
        // Create action list.
        let mut actions: EditScript<&str> = EditScript::new();
        let del1 = Delete::new(NodeId::new(3)); // INT 3
        let del2 = Delete::new(NodeId::new(4)); // INT 4
        let ins1 = Insert::new(n5, Some(NodeId::new(2)), 0);
        let ins2 = Insert::new(n6, Some(NodeId::new(2)), 1);
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
        // Apply action list.
        actions.apply(&mut arena).unwrap();
        let format2 = "\"Expr\" *
  \"INT\" 1
  \"Expr\" *
    \"INT\" 99
    \"INT\" 100
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    fn apply_to_list2() {
        let mut arena = create_arena();
        let format1 = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 3
    \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        let n5 = arena.new_node("INT", String::from("2"), None, None, None, None);
        // Create action list.
        let mut actions: EditScript<&str> = EditScript::new();
        let del = Delete { node: NodeId::new(4), }; // Remove "4".
        let ins = Insert { node: n5,
                           new_parent: Some(NodeId::new(2)),
                           nth_child: 0, };
        let upd = Update { // Change "+" to "*".
                           node: NodeId::new(0),
                           ty: "Expr",
                           label: String::from("*"), };
        actions.push(del);
        actions.push(ins);
        actions.push(upd);
        assert_eq!(3, actions.size());
        // Apply action list.
        actions.apply(&mut arena).unwrap();
        let format2 = "\"Expr\" *
  \"INT\" 1
  \"Expr\" *
    \"INT\" 2
    \"INT\" 3
";
        assert_eq!(format2, format!("{:?}", arena));
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
]",
        );
        assert_eq!(expected, actions.render_json(0));
    }
}

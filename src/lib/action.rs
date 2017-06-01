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
// if one is included with the Software (each a “Larger Work” to which the Software
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
use std::fmt::{Formatter, Display, Result};

use ast::{Arena, ArenaError, ArenaResult, NodeId};
use emitters::RenderJson;

/// Apply an action to an AST node.
pub trait ApplyAction<T: Clone + Display>: Display + RenderJson {
    /// Apply an action to an AST.
    fn apply(&mut self, arena: &mut Arena<T>) -> ArenaResult;
}

/// A list of actions to be applied.
///
/// This type will usually be used by iterating over the list and calling
/// `apply()` on each element.
pub struct EditScript<T> {
    actions: Vec<Box<ApplyAction<T>>>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
/// Delete a node from a given AST.
///
/// It is only valid to delete leaf nodes.
pub struct Delete {
    node: NodeId,
}

#[derive(Clone, Eq, PartialEq)]
/// Insert a new node into an AST as the `position`th child of an existing node.
pub struct Insert {
    /// New node in from-arena, must already exist before applying this trait.
    node: NodeId,
    nth_child: u16,
    /// New parent in to-arena.
    new_parent: NodeId,
}

#[derive(Copy, Clone, Eq, PartialEq)]
/// Move a node from one place to another within an AST.
pub struct Move {
    from_node: NodeId,
    parent: NodeId,
    pos: u16,
}

#[derive(Clone, Eq, PartialEq)]
/// Update the data inside an AST node.
pub struct Update<T: Clone> {
    node: NodeId,
    value: T,
    label: String,
}

impl Delete {
    /// Create a new `Delete` object.
    pub fn new(node: NodeId) -> Delete {
        Delete { node: node }
    }
}

impl Insert {
    /// Create a new `Insert` object.
    pub fn new(node: NodeId, parent: NodeId, nth: u16) -> Insert {
        Insert {
            node: node,
            new_parent: parent,
            nth_child: nth,
        }
    }
}

impl Move {
    /// Create a new `Move` object.
    pub fn new(from: NodeId, parent: NodeId, position: u16) -> Move {
        Move {
            from_node: from,
            parent: parent,
            pos: position,
        }
    }
}

impl<T: Display + Clone> Update<T> {
    /// Create a new `Update` object.
    pub fn new(node: NodeId, value: T, label: String) -> Update<T> {
        Update {
            node: node,
            value: value,
            label: label,
        }
    }
}

impl Display for Delete {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "DEL {}", self.node)
    }
}

impl Display for Insert {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f,
               "INS {} to {} at {}",
               self.node,
               self.new_parent,
               self.nth_child)
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f,
               "MOV {} to {} at {}",
               self.from_node,
               self.parent,
               self.pos)
    }
}

impl<T: Display + Clone> Display for Update<T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "UPD {} to {} {}", self.node, self.label, self.value)
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
        json.push(format!("{}\"parent\": {},", &ind_s, self.new_parent.id()));
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

impl<T: Clone> RenderJson for Update<T> {
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

impl<T: Clone + Display> ApplyAction<T> for Delete {
    fn apply(&mut self, arena: &mut Arena<T>) -> ArenaResult {
        debug_assert!(self.node.is_leaf(arena),
                      "Attempt to delete branch node {}",
                      self.node);
        self.node.detach(arena)
    }
}

impl<T: Clone + Display> ApplyAction<T> for Insert {
    fn apply(&mut self, arena: &mut Arena<T>) -> ArenaResult {
        self.node
            .make_nth_child_of(self.new_parent, self.nth_child, arena)
    }
}

impl<T: Clone + Display> ApplyAction<T> for Move {
    fn apply(&mut self, arena: &mut Arena<T>) -> ArenaResult {
        self.from_node
            .make_nth_child_of(self.parent, self.pos, arena)
    }
}

impl<T: Clone + Display> ApplyAction<T> for Update<T> {
    fn apply(&mut self, arena: &mut Arena<T>) -> ArenaResult {
        if !arena.contains(self.node) {
            return Err(ArenaError::NodeIdNotFound);
        }
        arena[self.node].value = self.value.clone();
        arena[self.node].label = self.label.clone();
        Ok(())
    }
}

impl<T: Clone> Default for EditScript<T> {
    fn default() -> EditScript<T> {
        EditScript { actions: vec![] }
    }
}

impl<T: Clone + Display> EditScript<T> {
    /// Create an empty list of actions.
    pub fn new() -> EditScript<T> {
        Default::default()
    }

    /// Push an action onto the action list.
    pub fn push<A: ApplyAction<T> + Display + 'static>(&mut self, action: A) {
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
}

impl<T: Display + Clone> Display for EditScript<T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for action in &self.actions {
            write!(f, "{:}\n", action)?
        }
        write!(f, "")
    }
}

impl<T: Clone> RenderJson for EditScript<T> {
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

impl<T: Clone + Display> ApplyAction<T> for EditScript<T> {
    fn apply(&mut self, arena: &mut Arena<T>) -> ArenaResult {
        for boxed_action in &mut self.actions {
            boxed_action.apply(arena)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn create_arena() -> Arena<String> {
        let mut arena = Arena::new();
        let root = arena.new_node(String::from("+"), String::from("Expr"), 0, None, None);
        let n1 = arena.new_node(String::from("1"), String::from("INT"), 2, None, None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node(String::from("*"), String::from("Expr"), 2, None, None);
        n2.make_child_of(root, &mut arena).unwrap();
        let n3 = arena.new_node(String::from("3"), String::from("INT"), 4, None, None);
        n3.make_child_of(n2, &mut arena).unwrap();
        let n4 = arena.new_node(String::from("4"), String::from("INT"), 4, None, None);
        n4.make_child_of(n2, &mut arena).unwrap();
        arena
    }

    #[test]
    fn fmt_delete() {
        let del = Delete::new(NodeId::new(2));
        assert_eq!("DEL 2", format!("{:}", del));
    }

    #[test]
    fn fmt_insert() {
        let ins = Insert::new(NodeId::new(3), NodeId::new(2), 0);
        assert_eq!("INS 3 to 2 at 0", format!("{:}", ins));
    }

    #[test]
    fn fmt_move() {
        let mov = Move::new(NodeId::new(3), NodeId::new(2), 0);
        assert_eq!("MOV 3 to 2 at 0", format!("{:}", mov));
    }

    #[test]
    fn fmt_update() {
        let upd = Update::new(NodeId::new(4), String::from("100"), String::from("INT"));
        assert_eq!("UPD 4 to INT 100", format!("{:}", upd));
    }

    #[test]
    fn apply_delete_leaf() {
        let mut arena = create_arena();
        let format1 = "Expr +
  INT 1
  Expr *
    INT 3
    INT 4
";
        assert_eq!(format1, format!("{}", arena));
        let mut del1 = Delete::new(NodeId::new(3));
        del1.apply(&mut arena).unwrap();
        let mut del2 = Delete::new(NodeId::new(4));
        del2.apply(&mut arena).unwrap();
        let format2 = "Expr +
  INT 1
  Expr *
";
        assert_eq!(format2, format!("{}", arena));
    }

    #[test]
    #[should_panic]
    fn apply_delete_branch() {
        let mut arena = create_arena();
        let format1 = "Expr +
  INT 1
  Expr *
    INT 3
    INT 4
";
        assert_eq!(format1, format!("{}", arena));
        let mut del = Delete::new(NodeId::new(2));
        del.apply(&mut arena).unwrap();
    }

    #[test]
    fn apply_insert() {
        let mut arena = create_arena();
        let n5 = arena.new_node(String::from("100"), String::from("INT"), 4, None, None);
        let format1 = "Expr +
  INT 1
  Expr *
    INT 3
    INT 4
";
        assert_eq!(format1, format!("{}", arena));
        let mut ins = Insert::new(n5, NodeId::new(2), 0);
        ins.apply(&mut arena).unwrap();
        let format2 = "Expr +
  INT 1
  Expr *
    INT 100
    INT 3
    INT 4
";
        assert_eq!(format2, format!("{}", arena));
    }

    #[test]
    fn apply_move() {
        let mut arena = create_arena();
        let format1 = "Expr +
  INT 1
  Expr *
    INT 3
    INT 4
";
        assert_eq!(format1, format!("{}", arena));
        let mut mov = Move::new(NodeId::new(4), NodeId::new(2), 0);
        mov.apply(&mut arena).unwrap();
        let format2 = "Expr +
  INT 1
  Expr *
    INT 4
    INT 3
";
        assert_eq!(format2, format!("{}", arena));
    }

    #[test]
    fn apply_update() {
        let mut arena = create_arena();
        assert_eq!(5, arena.size());
        let format1 = "Expr +
  INT 1
  Expr *
    INT 3
    INT 4
";
        assert_eq!(format1, format!("{}", arena));
        let mut upd = Update::new(NodeId::new(2), String::from("+"), String::from("Expr"));
        upd.apply(&mut arena).unwrap();
        let format2 = "Expr +
  INT 1
  Expr +
    INT 3
    INT 4
";
        assert_eq!(format2, format!("{}", arena));
    }

    #[test]
    fn apply_to_list1() {
        let mut arena = create_arena();
        let format1 = "Expr +
  INT 1
  Expr *
    INT 3
    INT 4
";
        let n5 = arena.new_node(String::from("100"), String::from("INT"), 4, None, None);
        let n6 = arena.new_node(String::from("99"), String::from("INT"), 4, None, None);
        assert_eq!(format1, format!("{}", arena));
        // Create action list.
        let mut actions: EditScript<String> = EditScript::new();
        let del1 = Delete::new(NodeId::new(3)); // INT 3
        let del2 = Delete::new(NodeId::new(4)); // INT 4
        let ins1 = Insert::new(n5, NodeId::new(2), 0);
        let ins2 = Insert::new(n6, NodeId::new(2), 1);
        let mov = Move::new(NodeId::new(6), NodeId::new(2), 0); // Swap "INT 100" and "INT 99".
        // Change "+"" to "*".
        let upd = Update::new(NodeId::new(0), String::from("*"), String::from("Expr"));
        actions.push(del1);
        actions.push(del2);
        actions.push(ins1);
        actions.push(ins2);
        actions.push(mov);
        actions.push(upd);
        // Apply action list.
        actions.apply(&mut arena).unwrap();
        let format2 = "Expr *
  INT 1
  Expr *
    INT 99
    INT 100
";
        assert_eq!(format2, format!("{}", arena));
    }

    #[test]
    fn apply_to_list2() {
        let mut arena = create_arena();
        let format1 = "Expr +
  INT 1
  Expr *
    INT 3
    INT 4
";
        assert_eq!(format1, format!("{}", arena));
        let n5 = arena.new_node(String::from("2"), String::from("INT"), 4, None, None);
        // Create action list.
        let mut actions: EditScript<String> = EditScript::new();
        let del = Delete { node: NodeId::new(4) }; // Remove "4".
        let ins = Insert {
            node: n5,
            new_parent: NodeId::new(2),
            nth_child: 0,
        };
        let upd = Update {
            // Change "+" to "*".
            node: NodeId::new(0),
            value: String::from("*"),
            label: String::from("Expr"),
        };
        actions.push(del);
        actions.push(ins);
        actions.push(upd);
        // Apply action list.
        actions.apply(&mut arena).unwrap();
        let format2 = "Expr *
  INT 1
  Expr *
    INT 2
    INT 3
";
        assert_eq!(format2, format!("{}", arena));
    }

    #[test]
    fn render_json() {
        let mut actions: EditScript<String> = EditScript::new();
        let del1 = Delete::new(NodeId::new(3)); // INT 3
        let del2 = Delete::new(NodeId::new(4)); // INT 4
        let ins1 = Insert::new(NodeId::new(5), NodeId::new(2), 0);
        let ins2 = Insert::new(NodeId::new(6), NodeId::new(2), 1);
        let mov = Move::new(NodeId::new(6), NodeId::new(2), 0); // Swap "INT 100" and "INT 99".
        // Change "+"" to "*".
        let upd = Update::new(NodeId::new(0), String::from("*"), String::from("Expr"));
        actions.push(del1);
        actions.push(del2);
        actions.push(ins1);
        actions.push(ins2);
        actions.push(mov);
        actions.push(upd);
        let expected = String::from("\"actions\": [
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
        \"label\": \"Expr\"
    }
]");
        assert_eq!(expected, actions.render_json(0));
    }
}

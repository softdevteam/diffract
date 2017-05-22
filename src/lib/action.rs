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

// #![warn(missing_docs)]

use std::clone::Clone;
use std::fmt;

use ast::{Arena, ArenaError, ArenaResult, NodeId};

/// Apply an action to an AST node.
pub trait ApplyAction<T: Clone> {
    fn apply(&mut self, arena: &mut Arena<T>) -> ArenaResult;
}

/// A list of actions to be applied.
///
/// This type will usually be used by iterating over the list and calling
/// `apply()` on each element.
pub type ActionList<T> = Vec<Box<ApplyAction<T>>>;

/// Delete a node from a given AST.
pub struct Delete {
    node: NodeId,
}

/// Insert a new node into an AST as the `position`th child of an existing node.
pub struct Insert<T: Clone> {
    nth_child: u16,
    value: T,
    label: String,
    indent: u32,
    new_parent: NodeId,
}

/// Move a node from one place to another within an AST.
pub struct Move {
    from_node: NodeId,
    parent: NodeId,
    pos: u16,
}

/// Update the data inside an AST node.
pub struct Update<T: Clone> {
    node: NodeId,
    value: T,
    label: String,
}

impl fmt::Display for Delete {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "DEL {}", self.node)
    }
}

impl<T: fmt::Display + Clone> fmt::Display for Insert<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let indent = " ".repeat(self.indent as usize);
        write!(f,
               "INS {}{} {} to {} at {}",
               indent,
               self.label,
               self.value,
               self.new_parent,
               self.nth_child)
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "MOV {} to {} at {}",
               self.from_node,
               self.parent,
               self.pos)
    }
}

impl<T: fmt::Display + Clone> fmt::Display for Update<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "UPD {} to {} {}", self.node, self.label, self.value)
    }
}

impl<T: Clone> ApplyAction<T> for Delete {
    fn apply(&mut self, arena: &mut Arena<T>) -> ArenaResult {
        self.node.detach_with_children(arena)
    }
}

impl<T: Clone> ApplyAction<T> for Insert<T> {
    fn apply(&mut self, arena: &mut Arena<T>) -> ArenaResult {
        let mut new_id = arena.new_node(self.value.clone(), self.label.clone(), self.indent);
        new_id.make_nth_child_of(self.new_parent, self.nth_child, arena)
    }
}

impl<T: Clone> ApplyAction<T> for Move {
    fn apply(&mut self, arena: &mut Arena<T>) -> ArenaResult {
        self.from_node
            .make_nth_child_of(self.parent, self.pos, arena)
    }
}

impl<T: Clone> ApplyAction<T> for Update<T> {
    fn apply(&mut self, arena: &mut Arena<T>) -> ArenaResult {
        if !arena.contains(self.node) {
            return Err(ArenaError::NodeIdNotFound);
        }
        arena[self.node].value = self.value.clone();
        arena[self.node].label = self.label.clone();
        Ok(())
    }
}

impl<T: Clone> ApplyAction<T> for ActionList<T> {
    fn apply(&mut self, arena: &mut Arena<T>) -> ArenaResult {
        // Iterating over indices here avoids having two mutable borrows.
        for action in self {
            action.apply(arena)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn create_arena() -> Arena<String> {
        let mut arena = Arena::new();
        let root = arena.new_node(String::from("+"), String::from("Expr"), 0);
        let n1 = arena.new_node(String::from("1"), String::from("INT"), 2);
        arena.make_child_of(n1, root).unwrap();
        let n2 = arena.new_node(String::from("*"), String::from("Expr"), 2);
        arena.make_child_of(n2, root).unwrap();
        let n3 = arena.new_node(String::from("3"), String::from("INT"), 4);
        arena.make_child_of(n3, n2).unwrap();
        let n4 = arena.new_node(String::from("4"), String::from("INT"), 4);
        arena.make_child_of(n4, n2).unwrap();
        arena
    }

    #[test]
    fn fmt_delete() {
        let del = Delete { node: NodeId::new(2) };
        assert_eq!("DEL 2", format!("{:}", del));
    }

    #[test]
    fn fmt_insert() {
        let ins = Insert {
            value: "100",
            label: String::from("INT"),
            indent: 4,
            new_parent: NodeId::new(2),
            nth_child: 0,
        };
        assert_eq!("INS     INT 100 to 2 at 0", format!("{:}", ins));
    }

    #[test]
    fn fmt_move() {
        let mov = Move {
            from_node: NodeId::new(3),
            parent: NodeId::new(2),
            pos: 0,
        };
        assert_eq!("MOV 3 to 2 at 0", format!("{:}", mov));
    }

    #[test]
    fn fmt_update() {
        let upd = Update {
            node: NodeId::new(4),
            value: String::from("100"),
            label: String::from("INT"),
        };
        assert_eq!("UPD 4 to INT 100", format!("{:}", upd));
    }

    #[test]
    fn apply_delete() {
        let mut arena = create_arena();
        let format1 = "Expr +
  INT 1
  Expr *
    INT 3
    INT 4
";
        assert_eq!(format1, format!("{}", arena));
        let mut del = Delete { node: NodeId::new(2) };
        del.apply(&mut arena).unwrap();
        let format2 = "Expr +
  INT 1
";
        assert_eq!(format2, format!("{}", arena));
    }

    #[test]
    fn apply_insert() {
        let mut arena = create_arena();
        let format1 = "Expr +
  INT 1
  Expr *
    INT 3
    INT 4
";
        assert_eq!(format1, format!("{}", arena));
        let mut ins = Insert {
            nth_child: 0,
            value: String::from("100"),
            label: String::from("INT"),
            indent: 4,
            new_parent: NodeId::new(2),
        };
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
        let mut mov = Move {
            from_node: NodeId::new(4),
            parent: NodeId::new(2),
            pos: 0,
        };
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
        let mut upd = Update {
            node: NodeId::new(2),
            value: String::from("+"),
            label: String::from("Expr"),
        };
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
        assert_eq!(format1, format!("{}", arena));
        // Create action list.
        let mut actions: ActionList<String> = vec![];
        let del1 = Delete { node: NodeId::new(3) }; // INT 3
        let del2 = Delete { node: NodeId::new(4) }; // INT 4
        let ins1 = Insert {
            value: String::from("100"),
            label: String::from("INT"),
            indent: 4,
            new_parent: NodeId::new(2),
            nth_child: 0,
        };
        let ins2 = Insert {
            value: String::from("99"),
            label: String::from("INT"),
            indent: 4,
            new_parent: NodeId::new(2),
            nth_child: 1,
        };
        let mov = Move {
            // Swap "INT 100" and "INT 99".
            from_node: NodeId::new(6),
            parent: NodeId::new(2),
            pos: 0,
        };
        let upd = Update {
            // Change "+"" to "*".
            node: NodeId::new(0),
            value: String::from("*"),
            label: String::from("Expr"),
        };
        actions.push(Box::new(del1));
        actions.push(Box::new(del2));
        actions.push(Box::new(ins1));
        actions.push(Box::new(ins2));
        actions.push(Box::new(mov));
        actions.push(Box::new(upd));
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
        // Create action list.
        let mut actions: ActionList<String> = vec![];
        let del = Delete { node: NodeId::new(2) }; // Remove "Expr *".
        let ins = Insert {
            value: String::from("2"),
            label: String::from("INT"),
            indent: 2,
            new_parent: NodeId::new(0),
            nth_child: 1,
        };
        let upd = Update {
            // Change "+" to "*".
            node: NodeId::new(0),
            value: String::from("*"),
            label: String::from("Expr"),
        };
        actions.push(Box::new(del));
        actions.push(Box::new(ins));
        actions.push(Box::new(upd));
        // Apply action list.
        actions.apply(&mut arena).unwrap();
        let format2 = "Expr *
  INT 1
  INT 2
";
        assert_eq!(format2, format!("{}", arena));
    }

}

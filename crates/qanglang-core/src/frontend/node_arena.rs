use crate::{
    arena::{Arena, Index},
    frontend::nodes::AstNode,
};

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq, Hash)]
pub struct NodeId(Index);

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq, Hash)]
pub struct NodeArrayId(Index);

pub struct NodeArena {
    pub node_id_arrays: NodeArrayArena,
    nodes: Arena<AstNode>,
}

impl NodeArena {
    pub fn new() -> Self {
        Self {
            nodes: Arena::new(),
            node_id_arrays: NodeArrayArena::new(),
        }
    }

    pub fn insert_node(&mut self, node: AstNode) -> NodeId {
        NodeId(self.nodes.insert(node))
    }

    pub fn get_node(&self, node_id: NodeId) -> &AstNode {
        &self.nodes[node_id.0]
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
struct NodeArrayChunk {
    ids: [NodeId; 8],
    size: usize,
    next_chunk: Option<NodeArrayId>,
}
struct NodeArrayArena {
    node_ids: Arena<NodeArrayChunk>,
}

impl NodeArrayArena {
    fn new() -> Self {
        Self {
            node_ids: Arena::new(),
        }
    }

    fn create(&mut self) -> NodeArrayId {
        let index = self.node_ids.insert(NodeArrayChunk {
            ids: std::array::from_fn(|_| NodeId::default()),
            size: 0,
            next_chunk: None,
        });

        NodeArrayId(index)
    }

    fn insert_id(&mut self, array_id: NodeArrayId, node_id: NodeId) {
        let mut current_id = array_id;

        loop {
            let needs_new_chunk = {
                let chunk = &mut self.node_ids[current_id.0];

                if chunk.size < 8 {
                    chunk.ids[chunk.size] = node_id;
                    chunk.size += 1;
                    return;
                }

                if let Some(next_id) = chunk.next_chunk {
                    current_id = next_id;
                    false
                } else {
                    true
                }
            };

            if needs_new_chunk {
                let new_chunk_id = self.create();
                self.node_ids[current_id.0].next_chunk = Some(new_chunk_id);
                current_id = new_chunk_id;
            }
        }
    }

    fn iter(&self, array_id: NodeArrayId) -> NodeArrayIterator<'_> {
        NodeArrayIterator {
            arena: self,
            current_chunk: Some(array_id),
            current_index: 0,
        }
    }
}

pub struct NodeArrayIterator<'a> {
    arena: &'a NodeArrayArena,
    current_chunk: Option<NodeArrayId>,
    current_index: usize,
}

impl<'a> Iterator for NodeArrayIterator<'a> {
    type Item = NodeId;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let chunk_id = self.current_chunk?;
            let chunk = &self.arena.node_ids[chunk_id.0];

            if self.current_index < chunk.size {
                let node_id = chunk.ids[self.current_index];
                self.current_index += 1;
                return Some(node_id);
            }

            self.current_chunk = chunk.next_chunk;
            self.current_index = 0;

            if self.current_chunk.is_none() {
                return None;
            }
        }
    }
}

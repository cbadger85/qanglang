use crate::{
    arena::{Arena, Index},
    frontend::typed_node_arena::NodeId,
};

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq, Hash)]
pub struct NodeArrayId(Index);

#[derive(Debug, Default, Clone, Copy, PartialEq)]
struct NodeArrayChunk {
    ids: [NodeId; 8],
    size: usize,
    next_chunk: Option<NodeArrayId>,
}
pub struct NodeArrayArena {
    node_ids: Arena<NodeArrayChunk>,
}

impl NodeArrayArena {
    pub fn new() -> Self {
        Self {
            node_ids: Arena::new(),
        }
    }

    pub fn create(&mut self) -> NodeArrayId {
        let index = self.node_ids.insert(NodeArrayChunk {
            ids: std::array::from_fn(|_| NodeId::default()),
            size: 0,
            next_chunk: None,
        });

        NodeArrayId(index)
    }

    pub fn push(&mut self, array_id: NodeArrayId, node_id: NodeId) {
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

    pub fn size(&self, array_id: NodeArrayId) -> usize {
        let mut total_size = 0;
        let mut current_id = Some(array_id);

        while let Some(chunk_id) = current_id {
            let chunk = &self.node_ids[chunk_id.0];
            total_size += chunk.size;
            current_id = chunk.next_chunk;
        }

        total_size
    }

    pub fn get_node_id_at(&self, array_id: NodeArrayId, index: usize) -> Option<NodeId> {
        let mut current_id = Some(array_id);
        let mut current_offset = 0;

        while let Some(chunk_id) = current_id {
            let chunk = &self.node_ids[chunk_id.0];
            
            if index < current_offset + chunk.size {
                let local_index = index - current_offset;
                return Some(chunk.ids[local_index]);
            }
            
            current_offset += chunk.size;
            current_id = chunk.next_chunk;
        }

        None
    }

    pub fn iter(&self, array_id: NodeArrayId) -> NodeArrayIterator<'_> {
        // Find the last chunk for backward iteration
        let mut last_chunk = array_id;
        while let Some(next) = self.node_ids[last_chunk.0].next_chunk {
            last_chunk = next;
        }
        let back_index = if self.node_ids[last_chunk.0].size > 0 {
            Some(self.node_ids[last_chunk.0].size - 1)
        } else {
            None
        };

        NodeArrayIterator {
            arena: self,
            current_chunk: Some(array_id),
            current_index: 0,
            back_chunk: Some(last_chunk),
            back_index,
        }
    }
}

pub struct NodeArrayIterator<'a> {
    arena: &'a NodeArrayArena,
    current_chunk: Option<NodeArrayId>,
    current_index: usize,
    back_chunk: Option<NodeArrayId>,
    back_index: Option<usize>,
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

impl<'a> DoubleEndedIterator for NodeArrayIterator<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        loop {
            let back_chunk_id = self.back_chunk?;
            let back_index = self.back_index?;

            // Check if we've met the forward iterator
            if Some(back_chunk_id) == self.current_chunk && back_index < self.current_index {
                return None;
            }

            let chunk = &self.arena.node_ids[back_chunk_id.0];
            let node_id = chunk.ids[back_index];

            // Move to previous element
            if back_index > 0 {
                self.back_index = Some(back_index - 1);
            } else {
                // Find previous chunk
                let mut prev_chunk = None;
                let mut current = self.current_chunk;

                while let Some(chunk_id) = current {
                    let chunk = &self.arena.node_ids[chunk_id.0];
                    if chunk.next_chunk == Some(back_chunk_id) {
                        prev_chunk = Some(chunk_id);
                        break;
                    }
                    current = chunk.next_chunk;
                }

                if let Some(prev_chunk_id) = prev_chunk {
                    let prev_chunk = &self.arena.node_ids[prev_chunk_id.0];
                    self.back_chunk = Some(prev_chunk_id);
                    self.back_index = if prev_chunk.size > 0 {
                        Some(prev_chunk.size - 1)
                    } else {
                        None
                    };
                } else {
                    self.back_chunk = None;
                    self.back_index = None;
                }
            }

            return Some(node_id);
        }
    }
}

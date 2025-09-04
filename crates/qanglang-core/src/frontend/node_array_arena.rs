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
}

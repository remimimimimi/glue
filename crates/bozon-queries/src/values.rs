// macro_rules! gen_values {
//     ($($id:ident),+) => {
//         $(
//             #[derive(Copy, Clone, Debug, Hash, ParitalEq, Eq)]
//             pub struct $id(::salsa::InternId);
//
//             impl ::salsa::InternKey for $id {
//                 fn from_intern_id(id: ::salsa::InternId) -> Self {
//                     Self(id)
//                 }
//             }
//         ),+
//     };
// }

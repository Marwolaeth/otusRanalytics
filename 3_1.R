friends_df <- getFriends(fields = 'sex,bdate')$items

friends_groups <- map(
  friends_df$id, 
  function(x) {
    tryCatch(
      data.frame(
        friend_id = x,
        group = getGroups(user_id = x, extended = 1)$items$name
      ),
      error = function(e) {NULL}
    )
  }
)
# str(friends_groups, 1)
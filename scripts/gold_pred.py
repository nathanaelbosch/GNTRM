import pandas as pd
import tensorflow as tf
from sklearn.model_selection import train_test_split
import sys

FLAGS = None


def main(_):
    # Import data
    player_time = pd.read_csv('small_data/player_time.csv')
    match = pd.read_csv('small_data/match.csv')
    match = match[['match_id', 'radiant_win']]
    player_time = player_time.merge(match)

    train_match_ids = match.match_id.sample(frac=0.7, replace=False)
    df_train = player_time[player_time.match_id.isin(train_match_ids)]
    df_test = player_time[~player_time.match_id.isin(train_match_ids)]

    train_labels = pd.DataFrame({
        'win': df_train.radiant_win,
        'lose': 1 - df_train.radiant_win})
    train_data = df_train.drop(['match_id', 'radiant_win'], axis=1)

    test_labels = pd.DataFrame({
        'win': df_test.radiant_win,
        'lose': 1 - df_test.radiant_win})
    test_data = df_test.drop(['match_id', 'radiant_win'], axis=1)

    '''
    mnist.train
    mnist.test
    mnist.validation
    70/20/10?
    '''

    # Create the model
    n = len(train_data.columns)
    m = 2
    x = tf.placeholder(tf.float32, [None, n])
    W = tf.Variable(tf.zeros([n, m]))
    b = tf.Variable(tf.zeros([m]))
    y = tf.matmul(x, W) + b

    # Define loss and optimizer
    y_ = tf.placeholder(tf.float32, [None, m])

    # The raw formulation of cross-entropy,
    #
    #   tf.reduce_mean(-tf.reduce_sum(y_ * tf.log(tf.nn.softmax(y)),
    #                                 reduction_indices=[1]))
    #
    # can be numerically unstable.
    #
    # So here we use tf.nn.softmax_cross_entropy_with_logits on the raw
    # outputs of 'y', and then average across the batch.
    cross_entropy = tf.reduce_mean(
        tf.nn.softmax_cross_entropy_with_logits(labels=y_, logits=y))
    train_step = tf.train.GradientDescentOptimizer(0.5).minimize(cross_entropy)

    sess = tf.InteractiveSession()
    tf.global_variables_initializer().run()
    # Train
    for _ in range(100):
        sess.run(train_step, feed_dict={x: train_data, y_: train_labels})

    # Test trained model
    correct_prediction = tf.equal(tf.argmax(y, 1), tf.argmax(y_, 1))
    accuracy = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))
    print(sess.run(accuracy, feed_dict={x: test_data,
                                        y_: test_labels}))


if __name__ == '__main__':
    tf.app.run(main=main, argv=[sys.argv[0]])

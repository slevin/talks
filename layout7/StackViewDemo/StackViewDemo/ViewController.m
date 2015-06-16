//
//  ViewController.m
//  StackViewDemo
//
//  Created by Sean Levin on 6/15/15.
//
//

#import "ViewController.h"

@interface ViewController ()
@property (nonatomic) UIStackView *stackView;
@property (nonatomic) UILabel *label;
@property (nonatomic) UIImageView *imageView;
@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];

    /*
    self.stackView = [[UIStackView alloc] init];
    [self.view addSubview:self.stackView];
    
    self.label = [[UILabel alloc] init];
    self.label.text = @"First Name";
    
    self.imageView = [[UIImageView alloc] init];
    self.imageView.image = [UIImage imageNamed:@"super_sweet_avatar.jpg"];
    
    self.stackView.axis = UILayoutConstraintAxisHorizontal;
    [self.stackView addArrangedSubview:self.label];
    [self.stackView addArrangedSubview:self.imageView];
    */
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

@end
